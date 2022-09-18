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

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.DeepSeq (NFData)
import Control.Exception
import GHC.Generics
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec (SourcePos)
import Data.Text (Text, pack)

data Type
  = Int
  | Double
  | String
  | Type :-> Type
  | TypeVariable Name
  deriving (Eq, Show, Generic, NFData)

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
  freeTypeVariable Int = Set.empty
  freeTypeVariable String = Set.empty
  freeTypeVariable Double = Set.empty
  -- freeTypeVariable (Constructor _ _) = Set.empty
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
  pure $ TypeVariable $ prefix <> (tshow $ typeInstantiationSupply state)

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
unify _ Int Int = pure emptySubstitution
unify _ String String = pure emptySubstitution
unify _ Double Double = pure emptySubstitution
unify sourcePos a b = throw . TypeCheckingException sourcePos $ fold
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
      , u
      , " does not match "
      , tshow t
      ]
  | otherwise = pure $ Map.singleton u t

typeCheckExpr :: TypeEnv -> AnnotatedExpr SourcePos -> TypeInstantiation (Substitution, Type)
typeCheckExpr _ (AnnExprInt _ _) = pure (emptySubstitution, Int)
typeCheckExpr _ (AnnExprString _ _) = pure (emptySubstitution, String)
typeCheckExpr _ (AnnExprDouble _ _) = pure (emptySubstitution, Double)
typeCheckExpr _ (AnnExprConstructor _ _ arity) = do
  ty <- type_ arity
  pure (emptySubstitution, ty)
  where
    type_ :: Int -> TypeInstantiation Type
    type_ 0 = do
      typeVar <- newTypeVar "a"
      pure typeVar
    type_ n = do
      typeVar <- newTypeVar "a"
      recursedType <- type_ $ n - 1
      pure $ typeVar :-> recursedType
typeCheckExpr (TypeEnv env) (AnnExprVariable sourcePos name) =
  case Map.lookup name env of
    Nothing ->
      throw . TypeCheckingException sourcePos $
        "Unbound variable: " <> name <> ". Maybe add it to `primitiveTypes`?"
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
-- TODO: let expressions should (like functions) create a bunch of type variables at the beginning then unify all of them mutually
typeCheckExpr env (AnnExprLet _ name expr1 expr2) = do
  (subs1, type1) <- typeCheckExpr env expr1
  let TypeEnv env1 = remove env name
      generalizedType = generalize (apply subs1 env) type1
      env2 = TypeEnv $ Map.insert name generalizedType env1
  (subs2, type2) <- typeCheckExpr (apply subs1 env2) expr2
  pure (subs1 `combineSubstitutions` subs2, type2)
-- TODO: typecheck that all case alternatives unify to the same result type
typeCheckExpr env (AnnExprCase sourcePos expr alts) = do
  let altExprs = map (\(_, _, expr) -> expr) alts
      firstAltExpr = (\(_, _, expr) -> expr) $ head alts
  altExprTypes :: [(Substitution, Type)] <- mapM (typeCheckExpr env) altExprs
  firstExpr <- typeCheckExpr env firstAltExpr
  foldM foldAction firstExpr altExprTypes
  where
    foldAction :: (Substitution, Type) -> (Substitution, Type) -> TypeInstantiation (Substitution, Type)
    foldAction (s1, t1) (s2, t2) = do
      newSubs <- unify sourcePos t1 t2
      pure (newSubs, t1)

infer :: Map Name Scheme -> AnnotatedExpr SourcePos -> TypeInstantiation Type
infer env expr = do
  (subs, type_) <- typeCheckExpr (TypeEnv env) expr
  pure $ apply subs type_

typeInference :: Program SourcePos -> IO (Either Text Type, TypeInstantiationState)
typeInference program =
  runTypeInstantiation $ infer (Map.fromList $ map (\(name, ty) -> (name, Scheme [] ty)) $ Map.toList primitiveTypes <> initialFunctionTypes program 1) (getMainExpr program)
  where
    initialFunctionTypes :: Program SourcePos -> Int -> [(Name, Type)]
    initialFunctionTypes (Program [] datas) _ = []
    initialFunctionTypes (Program (Function _ name args body : restFuncs) datas) n =
      (name, functionType n (length args)) : initialFunctionTypes (Program restFuncs datas) (n+1)

    functionType :: Int -> Arity -> Type
    functionType n 0 = do
      TypeVariable $ "f" <> tshow n <> "arg0"
    functionType n args = do
      (TypeVariable $ "f" <> tshow n <> "arg" <> tshow args) :-> (functionType n (args - 1))

    getMainExpr :: Program SourcePos -> AnnotatedExpr SourcePos
    getMainExpr (Program [] datas) = error "Couldn't find main"
    getMainExpr (Program (Function _ name _ body : restFuncs) datas) = case name of
      "main" -> body
      _ -> getMainExpr $ Program restFuncs datas

primitiveTypes :: Map Name Type
primitiveTypes = Map.fromList
  -- prim25 = "Bool" for now
  [ ("+", Int :-> Int :-> Int )
  , ("+.", Double :-> Double :-> Double )
  , ("if", TypeVariable "prim25" :-> TypeVariable "prim5" :-> TypeVariable "prim6")
  , ("==", TypeVariable "prim7" :-> TypeVariable "prim8" :-> TypeVariable "prim25")
  , ("-", TypeVariable "prim9" :-> TypeVariable "prim9" :-> TypeVariable "prim9")
  , ("||", TypeVariable "prim25" :-> TypeVariable "prim25" :-> TypeVariable "prim25")
  , ("<", TypeVariable "prim10" :-> TypeVariable "prim10" :-> TypeVariable "prim25")
  , ("/", TypeVariable "prim11" :-> TypeVariable "prim11" :-> TypeVariable "prim11")
  , ("const", TypeVariable "prim13" :-> TypeVariable "prim14" :-> TypeVariable "prim13")
  , ("const2", TypeVariable "prim15" :-> TypeVariable "prim16" :-> TypeVariable "prim16")
  , ("~", TypeVariable "prim17" :-> TypeVariable "prim17")
  , ("*", Int :-> Int :-> Int)
  , ("negate", TypeVariable "prim18" :-> TypeVariable "prim18")
  , ("twice", (TypeVariable "prim19" :-> TypeVariable "prim19") :-> TypeVariable "prim19")
  , ("id", TypeVariable "prim20" :-> TypeVariable "prim20")
  , (">", TypeVariable "prim21" :-> TypeVariable "prim21" :-> TypeVariable "prim25")
  , ("compose", (TypeVariable "prim23" :-> TypeVariable "prim24") :-> (TypeVariable "prim22" :-> TypeVariable "prim23") :-> TypeVariable "prim22" :-> TypeVariable "prim24")
  , ("<=", TypeVariable "prim27" :-> TypeVariable "prim27" :-> TypeVariable "prim25")
  , ("!=", TypeVariable "prim28" :-> TypeVariable "prim28" :-> TypeVariable "prim25")
  , (">=", TypeVariable "prim29" :-> TypeVariable "prim29" :-> TypeVariable "prim25")
  , ("&&", TypeVariable "prim25" :-> TypeVariable "prim25" :-> TypeVariable "prim25")
  ]
