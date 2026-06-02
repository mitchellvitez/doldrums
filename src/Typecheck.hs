{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typecheck
  ( typeInference
  , TypeCheckingException(..)
  , TypeInstantiationState(..)
  , Type
  )
where

import Language
import FixAst (singleExprForm)

import Control.Monad (foldM, forM)
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
import Text.Megaparsec (SourcePos, sourceLine, sourceColumn, unPos)
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
  | TypeVariable Name
  deriving (Eq, Show, Generic)

infixr 6 :->

-- most of this implementation is from the paper 'Algorithm W Step by Step' by Martin Grabmüller
class Types a where
  freeTypeVariable :: a -> Set Name
  apply :: Substitution -> a -> a

-- used for finding type variable replacements / generalization
data Scheme = Scheme [Name] Type

instance Types Scheme where
  freeTypeVariable (Scheme vars t) = freeTypeVariable t Set.\\ Set.fromList vars
  apply subs (Scheme vars t) = Scheme vars $ apply (foldr Map.delete subs vars) t

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
generalize env t = Scheme vars t
  where vars = Set.toList $ freeTypeVariable t Set.\\ freeTypeVariable env

instance Types TypeEnv where
  freeTypeVariable (TypeEnv env) = freeTypeVariable $ Map.elems env
  apply subs (TypeEnv env) = TypeEnv $ Map.map (apply subs) env

instance Types Type where
  freeTypeVariable (TypeVariable name) = Set.singleton name
  freeTypeVariable TypeInt = Set.empty
  freeTypeVariable TypeString = Set.empty
  freeTypeVariable TypeDouble = Set.empty
  freeTypeVariable (TypeTagged t) = Set.empty
  freeTypeVariable (a :-> b) = freeTypeVariable a `Set.union` freeTypeVariable b

  apply subs (TypeVariable name) = case Map.lookup name subs of
    Nothing -> TypeVariable name
    Just t -> t
  apply subs (a :-> b) = apply subs a :-> apply subs b
  apply subs t = t

instance Types a => Types [a] where
  freeTypeVariable list = foldr Set.union Set.empty $ map freeTypeVariable list
  apply subs = map $ apply subs

data TypeCheckingException = TypeCheckingException SourcePos Text
  deriving (Eq, Show)
instance Exception TypeCheckingException

data TypeInstantiationEnv = TypeInstantiationEnv

data TypeInstantiationState = TypeInstantiationState
  { typeInstantiationSupply :: Int
  , typeInstantiationSubstitution :: Substitution
  }
  deriving Show

-- our Type Instantiation monad stack
type TypeInstantiation a =
  ExceptT Text (ReaderT TypeInstantiationEnv (StateT TypeInstantiationState IO)) a

runTypeInstantiation :: TypeInstantiation a -> IO (Either Text a, TypeInstantiationState)
runTypeInstantiation t = do
  runStateT (runReaderT (runExceptT t) initialTypeInstantiationEnv) initialTypeInstantiationState
  where
    initialTypeInstantiationEnv = TypeInstantiationEnv
    initialTypeInstantiationState = TypeInstantiationState
      { typeInstantiationSupply = 0
      , typeInstantiationSubstitution = Map.empty
      }

newTypeVar :: Text -> TypeInstantiation Type
newTypeVar prefix = do
  state <- get
  put state { typeInstantiationSupply = typeInstantiationSupply state + 1 }
  pure . TypeVariable . Name $ prefix <> (tshow $ typeInstantiationSupply state)

instantiate :: Scheme -> TypeInstantiation Type
instantiate (Scheme vars t) = do
  newVars <- mapM (const $ newTypeVar "a") vars
  let subs = Map.fromList $ zip vars newVars
  pure $ apply subs t

unify :: SourcePos -> Type -> Type -> TypeInstantiation Substitution
unify sourcePos (a1 :-> b1) (a2 :-> b2) = do
  subs1 <- unify sourcePos a1 a2
  subs2 <- unify sourcePos (apply subs1 b1) (apply subs1 b2)
  pure $ subs1 `combineSubstitutions` subs2
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
typeCheckExpr _ (AnnExprLiteral _ (LiteralFloat _)) =
  pure (emptySubstitution, TypeDouble)
typeCheckExpr _ (AnnExprConstructor _ _ arity) = do
  ty <- type_ $ unArity arity
  pure (emptySubstitution, ty)
  where
    type_ :: Int -> TypeInstantiation Type
    type_ 0 = do
      -- TODO: replace this with the constructor Type
      typeVar <- newTypeVar "a"
      pure typeVar
    type_ n = do
      typeVar <- newTypeVar "a"
      recursedType <- type_ $ n - 1
      pure $ typeVar :-> recursedType
typeCheckExpr (TypeEnv env) (AnnExprVariable sourcePos name) =
  case Map.lookup name env of
    Nothing -> do
      ty <- newTypeVar "a"
      pure (emptySubstitution, ty)
      -- throw . TypeCheckingException sourcePos $
      --   "Unbound variable: " <> name <> ". Maybe add it to `primitiveTypes`?"
    Just ty -> do
      instantiatedType <- instantiate ty
      pure (emptySubstitution, instantiatedType)
typeCheckExpr env (AnnExprLambda _ name expr) = do
  typeVar <- newTypeVar "a"
  let TypeEnv env1 = remove env name
      env2 = TypeEnv $ env1 `Map.union` Map.singleton name (Scheme [] typeVar)
  (subs1, type1) <- typeCheckExpr env2 expr
  pure $ (subs1, apply subs1 typeVar :-> type1)
typeCheckExpr env (AnnExprApplication sourcePos expr1 expr2) = do
  typeVar <- newTypeVar "a"
  (subs1, type1) <- typeCheckExpr env expr1
  (subs2, type2) <- typeCheckExpr (apply subs1 env) expr2
  subs3 <- unify sourcePos (apply subs2 type1) (type2 :-> typeVar)
  pure (subs3 `combineSubstitutions` subs2 `combineSubstitutions` subs1, apply subs3 typeVar)
typeCheckExpr oldEnv (AnnExprLet sourcePos bindings expr2) = do
  newVars <- mapM (\name -> newTypeVar "a" >>= \var -> pure (name, Scheme [] var)) $ map fst bindings
  let env = oldEnv <> TypeEnv (Map.fromList newVars)
  ty <- newTypeVar "a"
  foldM (foldStep env) (emptySubstitution, ty) bindings
  where
    foldStep env (subs0, type0) (name, expr1) = do
      (subs1, type1) <- typeCheckExpr env expr1
      let TypeEnv env1 = remove env name
          generalizedType = generalize (apply subs1 env) type1
          env2 = TypeEnv $ Map.insert name generalizedType env1
      (subs2, type2) <- typeCheckExpr (apply subs1 env2) expr2
      pure (subs1 `combineSubstitutions` subs2, type2)
typeCheckExpr env (AnnExprCase sourcePos expr alts) = do
  let toNestedLambdas :: CaseAlternative SourcePos -> (Int, AnnotatedExpr SourcePos)
      toNestedLambdas (Alternative pattern body) =
        ( length $ patternNames pattern
        , foldr (\name expr -> AnnExprLambda sourcePos name expr) body $ patternNames pattern
        )
  let removeArgsFromType :: Int -> Type -> Type
      removeArgsFromType 0 t = t
      removeArgsFromType n (a :-> b) = removeArgsFromType (n-1) b
      removeArgsFromType _ _ = error "bad case pattern match"
  let altExprs = map toNestedLambdas alts
  altExprTypes@(firstAltExprType:_) :: [(Substitution, Type)] <- forM altExprs $ \(numArgs, expr) -> do
    (s, t) <- typeCheckExpr env expr
    pure (s, removeArgsFromType numArgs t)
  foldM foldAction firstAltExprType altExprTypes
  where
    foldAction :: (Substitution, Type) -> (Substitution, Type) -> TypeInstantiation (Substitution, Type)
    foldAction (s1, t1) (s2, t2) = do
      newSubs <- unify sourcePos t1 t2
      pure (newSubs, t1)

infer :: Map Name Scheme -> AnnotatedExpr SourcePos -> TypeInstantiation Type
infer env expr = do
  (subs, type_) <- typeCheckExpr (TypeEnv env) expr
  pure $ apply subs type_

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
  putTextLn $ Text.lines programText !! (unPos (sourceLine sourcePos) - 1)
  putStrLn $ line <> "^"
  putTextLn msg
  pure $ (Left "typechecking failed", TypeInstantiationState 0 Map.empty)
  exitFailure -- comment this out to treat typechecking as a warning

typeInference :: Program SourcePos -> Text -> IO (Either Text Type, TypeInstantiationState)
typeInference program programText = do
  -- convert to single-expression form (all top-level functions become lets in main) then typecheck
  (runTypeInstantiation . infer initialEnvironment $ singleExprForm program)
    `catch` typecheckingFailureHandler programText
  where
    initialEnvironment =
      Map.insert (Name "show") (Scheme [Name "a"] (TypeVariable (Name "a") :-> TypeString)) $
      Map.fromList $ map (\(name, ty) -> (name, Scheme [] ty)) $
      Map.toList primitiveTypes <> initialFunctionTypes program 1
    initialFunctionTypes :: Program SourcePos -> Int -> [(Name, Type)]
    initialFunctionTypes (Program [] []) _ = []
    initialFunctionTypes (Program (Function _ name args body : restFuncs) datas) n =
      (name, functionType n (Arity $ length args) Nothing) : initialFunctionTypes (Program restFuncs datas) (n+1)
    initialFunctionTypes (Program funcs (DataDeclaration [] dataTy : restDatas)) n =
      initialFunctionTypes (Program funcs restDatas) n
    initialFunctionTypes (Program funcs (DataDeclaration ((x, arity) : restDecls) dataTy : restDatas)) n =
      (Name $ unTag x, functionType n arity (Just dataTy)) : initialFunctionTypes (Program funcs (DataDeclaration restDecls dataTy : restDatas)) (n+1)

    functionType :: Int -> Arity -> Maybe DataType -> Type
    functionType n (Arity 0) mType = case mType of
      Nothing -> TypeVariable . Name $ "f" <> tshow n <> "arg0"
      Just dataTy -> TypeTagged dataTy
    functionType n (Arity numArgs) mType = do
      (TypeVariable . Name $ "f" <> tshow n <> "arg" <> tshow numArgs) :-> (functionType n (Arity $ numArgs - 1) mType)

-- TODO: add top-level type signatures like `if : Bool -> a -> a -> a` to build specifications like
-- `TypeSignature (Name "if") (TypeTagged (DataType "Bool") :-> tvar "prim31" :-> tvar "prim31" :-> tvar "prim31")
-- and check function types against them. list them all in Prelude.dol
primitiveTypes :: Map Name Type
primitiveTypes = Map.fromList
  [ (Name "+", TypeInt :-> TypeInt :-> TypeInt )
  , (Name "+.", TypeDouble :-> TypeDouble :-> TypeDouble )
  , (Name "==", tvar "prim7" :-> tvar "prim8" :-> TypeTagged (DataType "Bool"))
  , (Name "-", tvar "prim9" :-> tvar "prim9" :-> tvar "prim9")
  , (Name "||", TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool"))
  , (Name "<", tvar "prim10" :-> tvar "prim10" :-> TypeTagged (DataType "Bool"))
  , (Name "/", tvar "prim11" :-> tvar "prim11" :-> tvar "prim11")
  , (Name "const", tvar "prim13" :-> tvar "prim14" :-> tvar "prim13")
  , (Name "const2", tvar "prim15" :-> tvar "prim16" :-> tvar "prim16")
  , (Name "~", tvar "prim17" :-> tvar "prim17")
  , (Name "*", TypeInt :-> TypeInt :-> TypeInt)
  , (Name "*.", TypeDouble :-> TypeDouble :-> TypeDouble)
  , (Name "negate", tvar "prim18" :-> tvar "prim18")
  , (Name "twice", (tvar "prim19" :-> tvar "prim19") :-> tvar "prim19")
  , (Name "id", tvar "prim20" :-> tvar "prim20")
  , (Name ">", tvar "prim21" :-> tvar "prim21" :-> TypeTagged (DataType "Bool"))
  , (Name "compose", (tvar "prim23" :-> tvar "prim24") :-> (tvar "prim22" :-> tvar "prim23") :-> tvar "prim22" :-> tvar "prim24")
  , (Name "<=", tvar "prim27" :-> tvar "prim27" :-> TypeTagged (DataType "Bool"))
  , (Name "!=", tvar "prim28" :-> tvar "prim28" :-> TypeTagged (DataType "Bool"))
  , (Name ">=", tvar "prim29" :-> tvar "prim29" :-> TypeTagged (DataType "Bool"))
  , (Name "&&", TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool"))
  , (Name "and", TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool"))
  , (Name "not", TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool"))
  , (Name "or", TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool"))
  , (Name "xor", TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool") :-> TypeTagged (DataType "Bool"))
  , (Name "<>", TypeString :-> TypeString :-> TypeString)
  , (Name "compare", tvar "prim30" :-> tvar "prim30" :-> TypeTagged (DataType "Ordering"))
  , (Name "if", TypeTagged (DataType "Bool") :-> tvar "prim31" :-> tvar "prim31" :-> tvar "prim31")
  ]
  where tvar = TypeVariable . Name
