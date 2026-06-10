{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typecheck
  ( typeInference
  , TypeCheckingException(..)
  , TypeInstantiationState(..)
  , HoleInfo(..)
  , Type(..)
  , reportHoles
  )
where

import Language
import FixAst (singleExprForm)

import Data.List (find)
import Control.Monad (foldM, forM, forM_, when, unless)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Exception
import GHC.Generics
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec (SourcePos(..), sourceLine, sourceColumn, unPos, mkPos)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import System.Exit (exitFailure)

putTextLn :: Text -> IO ()
putTextLn = putStrLn . Text.unpack

data Type
  = TypeInt
  | TypeDouble
  | TypeString
  | TypeTagged DataType
  | Type :-> Type
  | TypeIO Type
  | TypeVariable Name
  deriving (Eq, Show, Generic)

infixr 6 :->

-- most of this implementation is from the paper 'Algorithm W Step by Step' by Martin Grabmüller
class Types a where
  freeTypeVariable :: a -> Set Name
  apply :: Substitution -> a -> a

data Constraint = Constraint Name Type
  deriving (Eq, Show, Generic)

instance Types Constraint where
  freeTypeVariable (Constraint _ t) = freeTypeVariable t
  apply subs (Constraint n t) = Constraint n $ apply subs t

-- used for finding type variable replacements / generalization
-- `forall a. a -> String`
-- is the same as
-- `Scheme [Name "a"] [] (TypeVariable (Name "a") :-> TypeString)`
data Scheme = Scheme [Name] [Constraint] Type

instance Types Scheme where
  freeTypeVariable (Scheme vars constraints t) =
    (freeTypeVariable t <> foldMap freeTypeVariable constraints) Set.\\ Set.fromList vars
  apply subs (Scheme vars constraints t) =
    Scheme vars (apply subs constraints) $ apply (foldr Map.delete subs vars) t

-- map from variables to their types, for substituting
type Substitution = Map Name Type

tshow :: Show a => a -> Text
tshow = pack . show

emptySubstitution :: Substitution
emptySubstitution = Map.empty

combineSubstitutions :: Substitution -> Substitution -> Substitution
combineSubstitutions a b = Map.map (apply a) b `Map.union` a

-- environment of names and the schemes they correspond to
newtype TypeEnv = TypeEnv (Map Name Scheme)

instance Semigroup TypeEnv where
  TypeEnv a <> TypeEnv b = TypeEnv (a <> b)

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars [] t
  where vars = Set.toList $ freeTypeVariable t Set.\\ freeTypeVariable env

instance Types TypeEnv where
  freeTypeVariable (TypeEnv env) = freeTypeVariable $ Map.elems env
  apply subs (TypeEnv env) = TypeEnv $ Map.map (apply subs) env

instance Types Type where
  freeTypeVariable (TypeVariable name) = Set.singleton name
  freeTypeVariable TypeInt = Set.empty
  freeTypeVariable TypeString = Set.empty
  freeTypeVariable TypeDouble = Set.empty
  freeTypeVariable (TypeTagged _) = Set.empty
  freeTypeVariable (TypeIO a) = freeTypeVariable a
  freeTypeVariable (a :-> b) = freeTypeVariable a `Set.union` freeTypeVariable b

  apply subs (TypeVariable name) = case Map.lookup name subs of
    Nothing -> TypeVariable name
    Just t -> t
  apply subs (a :-> b) = apply subs a :-> apply subs b
  apply subs (TypeIO a) = TypeIO (apply subs a)
  apply _ t = t

instance Types a => Types [a] where
  freeTypeVariable list = foldr Set.union Set.empty $ map freeTypeVariable list
  apply subs = map $ apply subs

data TypeCheckingException = TypeCheckingException SourcePos Text
  deriving (Eq, Show)
instance Exception TypeCheckingException

data TypeInstantiationEnv = TypeInstantiationEnv
  { signatureMap :: Map Name Scheme
  , instanceMap :: Map Name [Type]
  , envDataDeclarations :: [DataDeclaration]
  }

data HoleInfo = HoleInfo
  { holeName :: Name
  , holePos :: SourcePos
  , holeTypeVar :: Name
  }
  deriving Show

data TypeInstantiationState = TypeInstantiationState
  { typeInstantiationSupply :: Int
  , typeInstantiationSubstitution :: Substitution
  , typeConstraints :: [Constraint]
  , typedHoles :: [HoleInfo]
  }
  deriving Show

-- our Type Instantiation monad stack
type TypeInstantiation a =
  ExceptT Text (ReaderT TypeInstantiationEnv (StateT TypeInstantiationState IO)) a

runTypeInstantiation :: Map Name Scheme -> Map Name [Type] -> [DataDeclaration] -> TypeInstantiation a -> IO (Either Text a, TypeInstantiationState)
runTypeInstantiation signatures instances dataDecls t = do
  runStateT (runReaderT (runExceptT t) env) initialTypeInstantiationState
  where
    env = TypeInstantiationEnv { signatureMap = signatures, instanceMap = instances, envDataDeclarations = dataDecls }
    initialTypeInstantiationState = TypeInstantiationState
      { typeInstantiationSupply = 0
      , typeInstantiationSubstitution = Map.empty
      , typeConstraints = []
      , typedHoles = []
      }

addConstraints :: [Constraint] -> TypeInstantiation ()
addConstraints cs = modify $ \s -> s { typeConstraints = typeConstraints s <> cs }

newTypeVar :: Text -> TypeInstantiation Type
newTypeVar prefix = do
  curState <- get
  put curState { typeInstantiationSupply = typeInstantiationSupply curState + 1 }
  pure . TypeVariable . Name $ prefix <> (tshow $ typeInstantiationSupply curState)

instantiate :: Scheme -> TypeInstantiation (Type, [Constraint])
instantiate (Scheme vars constraints t) = do
  newVars <- mapM (const $ newTypeVar "a") vars
  let subs = Map.fromList $ zip vars newVars
  pure (apply subs t, apply subs constraints)

unify :: SourcePos -> Type -> Type -> TypeInstantiation Substitution
unify sourcePos (a1 :-> b1) (a2 :-> b2) = do
  subs1 <- unify sourcePos a1 a2
  subs2 <- unify sourcePos (apply subs1 b1) (apply subs1 b2)
  pure $ subs1 `combineSubstitutions` subs2
unify sourcePos (TypeIO a) (TypeIO b) = unify sourcePos a b
unify sourcePos (TypeVariable u) t = varBind sourcePos u t
unify sourcePos t (TypeVariable u) = varBind sourcePos u t
unify _ TypeInt TypeInt = pure emptySubstitution
unify _ TypeString TypeString = pure emptySubstitution
unify _ TypeDouble TypeDouble = pure emptySubstitution
unify sourcePos (TypeTagged (DataType a)) (TypeTagged (DataType b)) =
  if a == b
  then pure emptySubstitution
  else throwTypeCheckingException sourcePos a b
unify sourcePos a b = throwTypeCheckingException sourcePos a b

throwTypeCheckingException :: (Show a, Show b) => SourcePos -> a -> b -> TypeInstantiation Substitution
throwTypeCheckingException sourcePos a b =
  throw . TypeCheckingException sourcePos $ fold
    [ "Type mismatch: "
    , tshow a
    , " does not match "
    , tshow b
    ]

varBind :: SourcePos -> Name -> Type -> TypeInstantiation Substitution
varBind sourcePos u t
  | t == TypeVariable u = pure emptySubstitution
  | u `Set.member` freeTypeVariable t = throw . TypeCheckingException sourcePos $ fold
      [ "Occurs check failed: "
      , unName u
      , " does not match "
      , tshow t
      ]
  | otherwise = pure $ Map.singleton u t

typeCheckExpr :: TypeEnv -> AnnotatedExpr SourcePos -> TypeInstantiation (Substitution, Type)
typeCheckExpr _ (AnnExprLiteral _ (LiteralInt _)) =
  pure (emptySubstitution, TypeInt)
typeCheckExpr _ (AnnExprLiteral _ (LiteralString _)) =
  pure (emptySubstitution, TypeString)
typeCheckExpr _ (AnnExprLiteral _ (LiteralDouble _)) =
  pure (emptySubstitution, TypeDouble)
typeCheckExpr (TypeEnv envMap) (AnnExprConstructor _ tag _) =
  case Map.lookup (Name $ unTag tag) envMap of
    Just scheme -> do
      (instantiatedType, constraints) <- instantiate scheme
      addConstraints constraints
      pure (emptySubstitution, instantiatedType)
    Nothing -> do
      ty <- newTypeVar "a"
      pure (emptySubstitution, ty)
typeCheckExpr _ (AnnExprVariable sourcePos name)
  | "_" `Text.isPrefixOf` unName name = do
      ty <- newTypeVar "a"
      let
        tyVar = case ty of
          TypeVariable n -> n
          _ -> error "newTypeVar should return TypeVariable"
      modify $ \s -> s { typedHoles = HoleInfo name sourcePos tyVar : typedHoles s }
      pure (emptySubstitution, ty)
typeCheckExpr (TypeEnv env) (AnnExprVariable _ name) =
  case Map.lookup name env of
    Nothing -> do
      ty <- newTypeVar "a"
      pure (emptySubstitution, ty)
    Just scheme -> do
      (instantiatedType, constraints) <- instantiate scheme
      addConstraints constraints
      pure (emptySubstitution, instantiatedType)
typeCheckExpr env (AnnExprLambda _ name expr) = do
  typeVar <- newTypeVar "a"
  let TypeEnv env1 = remove env name
      env2 = TypeEnv $ env1 `Map.union` Map.singleton name (Scheme [] [] typeVar)
  (subs1, type1) <- typeCheckExpr env2 expr
  pure $ (subs1, apply subs1 typeVar :-> type1)
typeCheckExpr env (AnnExprApplication sourcePos expr1 expr2) = do
  typeVar <- newTypeVar "a"
  (subs1, type1) <- typeCheckExpr env expr1
  (subs2, type2) <- typeCheckExpr (apply subs1 env) expr2
  subs3 <- unify sourcePos (apply subs2 type1) (type2 :-> typeVar)
  pure (subs3 `combineSubstitutions` subs2 `combineSubstitutions` subs1, apply subs3 typeVar)
typeCheckExpr oldEnv (AnnExprLet sourcePos bindings expr2) = do
  sigMap <- asks signatureMap
  newVars <- mapM (\name -> newTypeVar "a" >>= \var -> pure (name, Scheme [] [] var)) $ map fst bindings
  let env = TypeEnv (Map.fromList newVars) <> oldEnv
  ty <- newTypeVar "a"
  foldM (foldStep sigMap env) (emptySubstitution, ty) bindings
  where
    foldStep sigMap env (_, _) (name, expr1) = do
      (subs1, type1) <- typeCheckExpr env expr1
      subsSig <- case Map.lookup name sigMap of
        Just sigScheme -> do
          (sigType, sigConstraints) <- instantiate sigScheme
          addConstraints sigConstraints
          unify sourcePos (apply subs1 type1) sigType
        Nothing -> pure emptySubstitution
      let subs1' = subsSig `combineSubstitutions` subs1
          TypeEnv env1 = remove env name
          generalizedType = generalize (apply subs1' env) (apply subs1' type1)
          env2 = TypeEnv $ Map.insert name generalizedType env1
      (subs2, type2) <- typeCheckExpr (apply subs1' env2) expr2
      pure (subs2 `combineSubstitutions` subs1', type2)
typeCheckExpr env (AnnExprCase sourcePos scrutinee alts) = do
  (subsScrutinee, scrutineeType) <- typeCheckExpr env scrutinee
  let env1 = apply subsScrutinee env

  dataDecls <- asks envDataDeclarations
  subsExhaustive <- checkCaseExhaustiveness sourcePos dataDecls scrutineeType alts scrutinee

  let env2 = apply subsExhaustive env1
      combinedSubs1 = subsExhaustive `combineSubstitutions` subsScrutinee

  let toNestedLambdas :: CaseAlternative SourcePos -> (Int, AnnotatedExpr SourcePos)
      toNestedLambdas (Alternative pattern body) =
        ( length $ patternNames pattern
        , foldr (\name altExpr -> AnnExprLambda sourcePos name altExpr) body $ patternNames pattern
        )
  let removeArgsFromType :: Int -> Type -> Type
      removeArgsFromType 0 t = t
      removeArgsFromType n (_ :-> b) = removeArgsFromType (n - 1) b
      removeArgsFromType _ _ = error "bad case pattern match"
  let altExprs = map toNestedLambdas alts
  altExprTypes@(firstAltExprType:_) :: [(Substitution, Type)] <- forM altExprs $ \(numArgs, altExpr) -> do
    (s, t) <- typeCheckExpr env2 altExpr
    pure (s, removeArgsFromType numArgs t)
  (subsAlts, resultType) <- foldM foldAction firstAltExprType altExprTypes
  pure (subsAlts `combineSubstitutions` combinedSubs1, resultType)
  where
    foldAction :: (Substitution, Type) -> (Substitution, Type) -> TypeInstantiation (Substitution, Type)
    foldAction (_, t1) (_, t2) = do
      newSubs <- unify sourcePos t1 t2
      pure (newSubs, t1)

checkCaseExhaustiveness :: SourcePos -> [DataDeclaration] -> Type -> [CaseAlternative a] -> AnnotatedExpr SourcePos -> TypeInstantiation Substitution
checkCaseExhaustiveness _ _ _ alts _ | any isCatchAll alts = pure emptySubstitution
  where
    isCatchAll (Alternative PatternWildcard _) = True
    isCatchAll (Alternative (PatternVar _) _) = True
    isCatchAll _ = False
checkCaseExhaustiveness pos dataDecls scrutineeType alts scrutinee = do
  let usedCtors = [tag | Alternative (PatternConstructor tag _) _ <- alts]
  if null usedCtors
    then pure emptySubstitution
    else do
      let knownDataType = case scrutineeType of
            TypeTagged dt -> find ((== dt) . dataType) dataDecls
            _ -> Nothing
          inferredDataType = case filter (\dd -> all (`elem` map fst (declarations dd)) usedCtors) dataDecls of
            [dd] -> Just dd
            _ -> Nothing
          dataDecl = case knownDataType of
            Just _ -> knownDataType
            Nothing -> inferredDataType
      case dataDecl of
        Just dd -> do
          subs <- case (scrutineeType, knownDataType) of
            (TypeVariable _, Nothing) -> unify pos scrutineeType (TypeTagged (dataType dd))
            _ -> pure emptySubstitution
          let allCtors = map fst (declarations dd)
              missingCtors = filter (`notElem` usedCtors) allCtors
              isOtherwiseGuard = case scrutinee of
                AnnExprVariable _ (Name "otherwise") -> True
                _ -> False
              isBoolTrueOnly = usedCtors == [Tag "True"] && missingCtors == [Tag "False"]
          unless (null missingCtors || (isOtherwiseGuard && isBoolTrueOnly)) $
            throw . TypeCheckingException pos $
              "Non-exhaustive patterns: "
              <> Text.intercalate ", " (map (\(Tag t) -> "'" <> t <> "'") missingCtors)
              <> " not covered"
          pure subs
        Nothing -> pure emptySubstitution

infer :: Map Name Scheme -> AnnotatedExpr SourcePos -> TypeInstantiation (Type, Substitution, [Constraint])
infer env expr = do
  (subs, type_) <- typeCheckExpr (TypeEnv env) expr
  constraints <- gets typeConstraints
  pure (apply subs type_, subs, constraints)

buildInstanceMap :: Program a -> Map Name [Type]
buildInstanceMap program =
  Map.fromListWith (<>) $
    [ (instanceClass inst, [typeHintToType typ])
    | inst <- instanceDeclarations program
    , typ <- instanceTypes inst
    ]

solveConstraints :: Substitution -> [Constraint] -> TypeInstantiation ()
solveConstraints subs constraints = do
  instances <- asks instanceMap
  let resolved = map (\(Constraint name typ) -> (name, apply subs typ)) constraints
  forM_ resolved $ \(name, typ) ->
    case Map.lookup name instances of
      Nothing -> throwError $ "No instances for " <> tshow name <> " " <> tshow typ
      Just candidates ->
        if typ `elem` candidates
        then pure ()
        else throwError $ "No instance for " <> tshow name <> " " <> tshow typ

resolveHoles :: Substitution -> [HoleInfo] -> [(Name, SourcePos, Type)]
resolveHoles subs = map (\(HoleInfo n p tv) -> (n, p, apply subs (TypeVariable tv)))

reportHoles :: Text -> Substitution -> [HoleInfo] -> IO ()
reportHoles programText subs holes = do
  let resolved = resolveHoles subs holes
  forM_ resolved $ \(name, sourcePos, ty) -> do
    putStrLn $ fold
      [ "Found typed hole `"
      , Text.unpack $ unName name
      , " :: "
      , Text.unpack $ pprintType ty
      , "` in the expression below starting at "
      , show . unPos $ sourceLine sourcePos
      , ":"
      , show . unPos $ sourceColumn sourcePos
      ]
    let line = replicate (unPos (sourceColumn sourcePos) - 1) '-'
    putStrLn $ line <> "v"
    let textLines = Text.lines programText
        lineNum = unPos (sourceLine sourcePos) - 1
    when (lineNum >= 0 && lineNum < length textLines) $
      putTextLn $ textLines !! lineNum
    putStrLn $ line <> "^"

pprintType :: Type -> Text
pprintType = \case
  TypeInt -> "Int"
  TypeDouble -> "Double"
  TypeString -> "String"
  TypeTagged (DataType n) -> n
  TypeIO a -> "IO " <> pprintType a
  TypeVariable (Name n) -> n
  a :-> b -> pprintType a <> " -> " <> pprintType b

typecheckingFailureHandler :: Text -> TypeCheckingException -> IO (Either Text Type, TypeInstantiationState)
typecheckingFailureHandler programText (TypeCheckingException sourcePos msg) = do
  putStrLn $ fold
    [ "Typechecking failed at "
    , show . unPos $ sourceLine sourcePos
    , ":"
    , show . unPos $ sourceColumn sourcePos
    , " in the expression"
    ]
  let line = replicate (unPos (sourceColumn sourcePos) - 1) '-'
  putStrLn $ line <> "v"
  let textLines = Text.lines programText
      lineNum = unPos (sourceLine sourcePos) - 1
  when (lineNum >= 0 && lineNum < length textLines) $
    putTextLn $ textLines !! lineNum
  putStrLn $ line <> "^"
  putTextLn msg
  exitFailure

typeHintToType :: TypeHint -> Type
typeHintToType = \case
  TypeHintInt -> TypeInt
  TypeHintDouble -> TypeDouble
  TypeHintString -> TypeString
  TypeHintVar n -> TypeVariable n
  TypeHintConstructor dt -> TypeTagged dt
  TypeHintApp dt _ -> TypeTagged dt
  a :~> b -> typeHintToType a :-> typeHintToType b
  TypeHintConstraint _ body -> typeHintToType body

typeHintFreeVars :: TypeHint -> [Name]
typeHintFreeVars = \case
  TypeHintVar n -> [n]
  a :~> b -> typeHintFreeVars a <> typeHintFreeVars b
  TypeHintApp _ args -> concatMap typeHintFreeVars args
  TypeHintConstraint constraints body ->
    concatMap (typeHintFreeVars . snd) constraints <> typeHintFreeVars body
  _ -> []

typeHintToScheme :: TypeHint -> Scheme
typeHintToScheme (TypeHintConstraint constraints body) =
  Scheme vars (map (\(cn, arg) -> Constraint cn (typeHintToType arg)) constraints) (typeHintToType body)
  where
    usedVars = typeHintFreeVars body <> concatMap (typeHintFreeVars . snd) constraints
    vars = Set.toList $ Set.fromList usedVars
typeHintToScheme hint =
  Scheme (typeHintFreeVars hint) [] (typeHintToType hint)

typeInference :: Program SourcePos -> Text -> IO (Either Text Type, TypeInstantiationState)
typeInference program programText = do
  (runTypeInstantiation sigSchemes insts (dataDeclarations program) $ do
    (typ, subs, constraints) <- infer initialEnvironment $ singleExprForm program
    solveConstraints subs constraints
    let ioUnit = TypeIO (TypeTagged (DataType "Unit"))
        appliedTyp = apply subs typ
    subs' <- unify (SourcePos "" (mkPos 1) (mkPos 1)) appliedTyp ioUnit
    let finalSubs = combineSubstitutions subs' subs
    modify $ \s -> s { typeInstantiationSubstitution = finalSubs }
    pure $ apply subs' appliedTyp)
    `catch` typecheckingFailureHandler programText
  where
    sigSchemes = Map.fromList . map (fmap typeHintToScheme) $ typeSignatures program
    insts = buildInstanceMap program
    initialEnvironment :: Map Name Scheme
    initialEnvironment =
      ioPrimitives <>
      sigSchemes <>
      Map.fromList (initialFunctionTypes program 1)

    ioPrimitives :: Map Name Scheme
    ioPrimitives = Map.fromList
      [ (Name ">>=", Scheme [Name "a", Name "b"] [] (TypeIO (TypeVariable (Name "a")) :-> ((TypeVariable (Name "a") :-> TypeIO (TypeVariable (Name "b"))) :-> TypeIO (TypeVariable (Name "b")))))
      , (Name "pure", Scheme [Name "a"] [] (TypeVariable (Name "a") :-> TypeIO (TypeVariable (Name "a"))))
      , (Name "print", Scheme [Name "a"] [] (TypeVariable (Name "a") :-> TypeIO (TypeTagged (DataType "Unit"))))
      , (Name "putStrLn", Scheme [] [] (TypeString :-> TypeIO (TypeTagged (DataType "Unit"))))
      , (Name "getLine", Scheme [] [] (TypeIO TypeString))
      , (Name "readFile", Scheme [] [] (TypeString :-> TypeIO TypeString))
      , (Name "writeFile", Scheme [] [] (TypeString :-> (TypeString :-> TypeIO (TypeTagged (DataType "Unit")))))
      ]

    initialFunctionTypes :: Program SourcePos -> Int -> [(Name, Scheme)]
    initialFunctionTypes (Program [] [] _ _ _) _ = []
    initialFunctionTypes (Program (Function _ name args _ : restFuncs) datas sigs cls instances) n =
      (name, Scheme [] [] $ functionType n (Arity $ length args) Nothing)
        : initialFunctionTypes (Program restFuncs datas sigs cls instances) (n+1)
    initialFunctionTypes (Program funcs (DataDeclaration [] _dataTy _typeParams _ : restDatas) sigs cls instances) n =
      initialFunctionTypes (Program funcs restDatas sigs cls instances) n
    initialFunctionTypes (Program funcs (DataDeclaration ((x, args) : restDecls) dataTy typeParams deriv : restDatas) sigs cls instances) n =
      (Name $ unTag x, constructorScheme typeParams (constructorArgTypes args) dataTy)
      : initialFunctionTypes (Program funcs (DataDeclaration restDecls dataTy typeParams deriv : restDatas) sigs cls instances) (n+1)

    constructorScheme :: [Name] -> [TypeRef] -> DataType -> Scheme
    constructorScheme typeParams typeRefs dataTy =
      Scheme schemeVars [] $ foldr refToFn (TypeTagged dataTy) typeRefs
      where
        schemeVars = typeParams <> [n | TypeRefVar n <- typeRefs]
        refToFn :: TypeRef -> Type -> Type
        refToFn (TypeRefVar name) = (TypeVariable name :->)
        refToFn (TypeRefConstructor dt) = (typeRefToType dt :->)
        refToFn (TypeRefApp dt _) = (typeRefToType dt :->)

        typeRefToType :: DataType -> Type
        typeRefToType (DataType "Int") = TypeInt
        typeRefToType (DataType "String") = TypeString
        typeRefToType (DataType "Double") = TypeDouble
        typeRefToType dt = TypeTagged dt

    functionType :: Int -> Arity -> Maybe DataType -> Type
    functionType n (Arity 0) mType = case mType of
      Nothing -> TypeVariable . Name $ "f" <> tshow n <> "arg0"
      Just dataTy -> TypeTagged dataTy
    functionType n (Arity numArgs) mType = do
      (TypeVariable . Name $ "f" <> tshow n <> "arg" <> tshow numArgs) :-> (functionType n (Arity $ numArgs - 1) mType)
