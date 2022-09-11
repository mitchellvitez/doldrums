{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module Typecheck where

import Language

import Control.Monad.Trans.State.Lazy
import Control.DeepSeq (NFData)
import Control.Exception
import GHC.Generics
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Kind hiding (Type)
import Data.Text (Text, unpack, pack)
import Data.Void
import Text.Megaparsec.Pos (SourcePos)

data Type
  = Bool
  | Int
  | Double
  | String
  | Constr
  | Type :-> Type
  | TypeVar TypeVariable
  deriving (Eq, Show, Generic, NFData)

infixr 6 :->

type TypeVarId = Int

data TypeVariable
  = Bound Type
  | Unbound TypeVarId
  deriving (Eq, Show, Generic, NFData)

data TypeCheckingException = TypeCheckingException Text
  deriving (Eq, Show)
instance Exception TypeCheckingException

-- data TExpr
--   = TExprVariable Type Name
--   | TExprInt Type Integer
--   | TExprBool Type Bool
--   | TExprDouble Type Double
--   | TExprString Type Text
--   | TExprConstructor Type Tag Arity
--   | TExprApplication Type TExpr TExpr
--   | TExprLet Type [(Name, TExpr)] TExpr
--   | TExprLambda Type [Name] TExpr
--   deriving (Show, Eq, Generic, NFData)

-- getType :: TExpr -> Type
-- getType = \case
--   TExprVariable t _ -> t
--   TExprInt t _ -> t
--   TExprBool t _ -> t
--   TExprDouble t _ -> t
--   TExprString t _ -> t
--   TExprConstructor t _ _ -> t
--   TExprApplication t _ _ -> t
--   TExprLet t _ _ -> t
--   TExprLambda t _ _ -> t

-- infer :: TExpr -> TExpr
-- infer = id

-- pretty print all type judgments for this AST
-- judgments :: TExpr -> [String]
-- judgments (TExprVariable t name) = [unpack name <> " : " <> show t]
-- judgments (TExprInt t n) = [show n <> " : " <> show t]
-- judgments (TExprBool t b) = [show b <> " : " <> show t]
-- judgments (TExprDouble t d) = [show d <> " : " <> show t]
-- judgments (TExprString t s) = [show s <> " : " <> show t]
-- judgments (TExprConstructor t tag arity) = ["Constr{" <> show tag <> "," <> show arity <> "}" <> " : " <> show t]
-- judgments (TExprApplication t e1 e2) = ["app" <> " : " <> show t] <> judgments e1 <> judgments e2
-- judgments (TExprLet t bindings expr) = ["let" <> " : " <> show t] <> concatMap (\(name, expr) -> judgments expr) bindings <> judgments expr
-- judgments (TExprLambda t names expr) = ["lam" <> " : " <> show t] <> judgments expr

-- data TypeJudgmentState = TypeJudgmentState
--   { nextTypeVarId :: TypeVarId
--   , bindings :: Map Name Type
--   }

-- isUnboundVar :: Type -> Bool
-- isUnboundVar (TypeVar (Unbound _)) = True
-- isUnboundVar _ = False

-- toTypedAST :: Expr -> State TypeJudgmentState TExpr
-- -- toTypedAST (ExprVariable "+") = pure $ TExprVariable (Int :-> Int :-> Int) "+"
-- toTypedAST (ExprVariable name) = do
--   state <- get
--   case Map.lookup name (bindings state) of
--     Just ty ->
--       pure $ TExprVariable (TypeVar $ Bound ty) name
--     Nothing -> do
--       put $ state { nextTypeVarId = nextTypeVarId state + 1}
--       pure $ TExprVariable (TypeVar . Unbound $ nextTypeVarId state) name
-- toTypedAST (ExprInt n)      = pure $ TExprInt      Int     n
-- toTypedAST (ExprBool b)     = pure $ TExprBool     Bool    b
-- toTypedAST (ExprString s)   = pure $ TExprString   String  s
-- toTypedAST (ExprDouble d)   = pure $ TExprDouble   Double  d
-- toTypedAST (ExprConstructor tag arity) = undefined -- TODO
-- toTypedAST (ExprApplication e1 e2) = do
--   typedE1 <- toTypedAST e1
--   typedE2 <- toTypedAST e2
--   case getType typedE1 of
--     -- (TypeVar (Bound ty)) -> pure $ TExprApplication ty typedE1 typedE2
--     (TypeVar (Unbound x)) -> pure $ TExprApplication (TypeVar (Unbound x)) typedE1 typedE2
--     (a :-> b) -> do
--       if a == getType typedE2 || isUnboundVar a || isUnboundVar (getType typedE2)
--       then pure $ TExprApplication b typedE1 typedE2
--       else throw . TypeCheckingException . pack $ "Type mismatch: " <> show a <> ", " <> show e2
--     x -> throw $ TypeCheckingException $ "Trying to apply something that isn't a function: " <> pack (show x)
-- toTypedAST (ExprLet bindings body) = do
--   typedBody <- toTypedAST body
--   typedBindings <- mapM (\(name, expr) -> do
--     typedExpr <- toTypedAST expr
--     pure (name, typedExpr)) bindings
--   pure $ TExprLet (getType typedBody) typedBindings typedBody
-- toTypedAST (ExprLambda bindings body) = do
--   typedBody <- toTypedAST body
--   pure $ TExprLambda (getType typedBody) bindings typedBody

-- typecheck :: Program -> [(Name, SimpleType)]
-- typecheck program =
--   Map.toList . snd $ runState (typecheckProgram prog) (Map.fromList [])
--   where prog = Map.fromList $ map (\(name, args, body) -> (name, (args, body))) program

-- typecheckProgram :: Map Name ([Name], Annotated Expr) -> State (Map Name SimpleType) ()
-- typecheckProgram map = undefined

-- typecheckArithmeticOperator :: String -> Expr -> Expr -> Expr -> SourcePos -> Annotation -> Typed Expr
-- typecheckArithmeticOperator name e a b pos annotation = case (typecheckExpr [] (Annotated a annotation), typecheckExpr [] (Annotated b annotation)) of
--     (Typed _ Int    , Typed _ Int)     -> Typed e Int
--     (Typed _ Int    , Typed _ Float)   -> Typed e Float
--     (Typed _ Float  , Typed _ Int)     -> Typed e Float
--     (Typed _ Float  , Typed _ Float)   -> Typed e Float
--     (Typed _ (Unknown _), Typed _ Int)     -> Typed e Int
--     (Typed _ Int    , Typed _ (Unknown _)) -> Typed e Int
--     (Typed _ (Unknown a), Typed _ (Unknown b)) -> if a == b then Typed e (Unknown a) else throw $ TypeCheckingException pos $ show a <> " does not match " <> show b
--     (Typed _ (Unknown _), Typed _ Float)   -> Typed e Float
--     (Typed _ Float  , Typed _ (Unknown _)) -> Typed e Float
--     (Typed _ t1     , Typed _ (Unknown _)) -> throw . TypeCheckingException pos $
--       "Invalid first argument type for " <> name <> ": " <> show t1
--     (Typed _ (Unknown _), Typed _ t2)      -> throw . TypeCheckingException pos $
--       "Invalid second argument type for " <> name <> ": " <> show t2
--     (Typed _ t1     , Typed _ t2)      -> throw . TypeCheckingException pos $
--       "Invalid argument types for " <> name <> ": " <> show t1 <> ", " <> show t2

-- typecheckExpr :: [Name] -> Annotated Expr -> Typed Expr
-- typecheckExpr (x:xs) annot@(Annotated expr _) = Typed expr ((exprType $ typecheckExpr [] annot) :-> (exprType $ typecheckExpr xs annot))
-- typecheckExpr [] (Annotated expr annotation@(Annotation pos)) = case expr of
--   e@(ExprLiteral (ValueInt n))    -> Typed e Int
--   e@(ExprLiteral (ValueDouble n)) -> Typed e Float
--   e@(ExprLiteral (ValueBool b))   -> Typed e Bool
--   e@(ExprLiteral (ValueString s)) -> Typed e String

--   e@(ExprApplication (ExprApplication (ExprVariable "+") a) b) -> typecheckArithmeticOperator "+" e a b pos annotation
--   e@(ExprApplication (ExprApplication (ExprVariable "*") a) b) -> typecheckArithmeticOperator "*" e a b pos annotation
--   e@(ExprApplication (ExprApplication (ExprVariable "/") a) b) -> typecheckArithmeticOperator "/" e a b pos annotation
--   e@(ExprApplication (ExprApplication (ExprVariable "-") a) b) -> typecheckArithmeticOperator "-" e a b pos annotation

--   -- TODO: if variable in list of known variables, use that type, otherwise assign a new TUnknown type to it
--   e@(ExprVariable name)           -> Typed e (Unknown "a") -- TODO: don't just use `a`
--   e@(ExprConstructor tag arity)   -> Typed e Constr
--   e@(ExprLambda vars body) ->
--     case typecheckExpr [] (Annotated body annotation) of
--       Typed _ t -> Typed e t
--   e@(ExprApplication e1 e2) ->
--     case e1 of
--       ExprLiteral v -> throw $ TypeCheckingException pos $ "Trying to apply a literal: " <> show v
--       ExprConstructor _ _ -> throw $ TypeCheckingException pos "Trying to apply a constructor"
--       _ -> case (typecheckExpr [] (Annotated e1 annotation), typecheckExpr [] (Annotated e2 annotation)) of
--         (Typed _ t1, Typed _ t2) -> case (t1, t2) of
--           (Unknown a, t) -> Typed e t
--           (t, Unknown a) -> Typed e t
--           (a, b) -> if a == b
--             then Typed e a
--             else throw $ TypeCheckingException pos $ show a <> " does not match " <> show b
--   e@(ExprLet bindings body) ->
--     case typecheckExpr [] (Annotated body annotation) of
--       Typed _ t -> Typed e t
