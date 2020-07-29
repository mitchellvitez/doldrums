module Typecheck where

import Language

import Control.Exception
import Data.Constraint
import Data.Functor.Product
import Data.Kind hiding (Type)
import Data.Text (Text)
import Data.Void

data TypedExpr ty where
  TypedExprVariable :: Name -> TypedExpr ty
  TypedExprLiteral :: ty -> TypedExpr ty
  TypedExprLet :: [(Name, TypedExpr ty1)] -> TypedExpr t2 -> TypedExpr ty
  TypedExprConstructor :: Int -> Int -> TypedExpr ty
  TypedExprApplication :: TypedExpr ty1 -> TypedExpr ty2 -> TypedExpr ty
  TypedExprLambda :: [Name] -> TypedExpr ty

data Op
  = Binary BinOp
  | Unary UnOp
  deriving (Show, Eq)

data UnOp
  = Neg -- ~
  | Not -- !
  deriving (Show, Eq)

data BinOp
  = Ap  -- $
  | Or  -- ||
  | And -- &&
  | Geq -- >=
  | Leq -- <=
  | Eq  -- ==
  | Neq -- !=
  | Gt  -- >
  | Lt  -- <
  | Add -- +
  | Sub -- -
  | Mul -- *
  | Div -- div
  | AddFloat -- +.
  | SubFloat -- -.
  | MulFloat -- *.
  | DivFloat -- /.
  deriving (Show, Eq)

data TypedOp inTy outTy
  = TBinary (TypedBinOp inTy outTy)
  | TUnary  (TypedUnOp  inTy outTy)

data TypedBinOp inTy outTy where
  TAp  :: TypedBinOp inTy outTy -- TODO check this
  TOr  :: TypedBinOp Bool Bool
  TAnd :: TypedBinOp Bool Bool
  TGeq :: Ord inTy => TypedBinOp inTy Bool
  TLeq :: Ord inTy => TypedBinOp inTy Bool
  TEq  :: Eq  inTy => TypedBinOp inTy Bool
  TNeq :: Eq  inTy => TypedBinOp inTy Bool
  TGt  :: Ord inTy => TypedBinOp inTy Bool
  TLt  :: Ord inTy => TypedBinOp inTy Bool
  TAdd :: Num inTy => TypedBinOp inTy inTy
  TSub :: Num inTy => TypedBinOp inTy inTy
  TMul :: Num inTy => TypedBinOp inTy inTy
  TDiv :: Num inTy => TypedBinOp inTy inTy
  TAddFloat :: Num inTy => TypedBinOp inTy inTy
  TSubFloat :: Num inTy => TypedBinOp inTy inTy
  TMulFloat :: Num inTy => TypedBinOp inTy inTy
  TDivFloat :: Num inTy => TypedBinOp inTy inTy

data TypedUnOp inTy outTy where
  TNot :: TypedUnOp Bool Bool
  TNeg :: Num ty => TypedUnOp ty ty

data Type ty where
  Bool :: Type Bool
  Int :: Type Integer
  Double :: Type Double
  String :: Type Text
  Constr :: Type Void
  Other :: Type Void

type TypedExpression = Product TypedExpr Type

data A (f :: * -> *) = forall x. A (f x)

data SimpleType
  = TBool
  | TInt
  | TDouble
  | TString
  | TConstr
  | TUnknown
  deriving (Eq, Show)

data Typed e
  = Typed e SimpleType
  | Fail
  deriving (Eq, Show)

data TypeCheckingException = TypeCheckingException
  deriving (Eq, Show)
instance Exception TypeCheckingException

typedcheck :: Expr -> Typed Expr
typedcheck expr = case expr of
  e@(ExprLiteral (ValueInt n))    -> Typed e TInt
  e@(ExprLiteral (ValueDouble n)) -> Typed e TDouble
  e@(ExprLiteral (ValueBool b))   -> Typed e TBool
  e@(ExprLiteral (ValueString s)) -> Typed e TString
  e@(ExprVariable name)           -> Typed e TUnknown
  e@(ExprConstructor tag arity)   -> Typed e TConstr
  e@(ExprLambda vars body) ->
    case typedcheck body of
      Typed _ t -> Typed e t
      Fail -> Fail
  e@(ExprApplication e1 e2) ->
    case e1 of
      ExprLiteral _ -> Fail
      ExprConstructor _ _ -> Fail
      _ -> case (typedcheck e1, typedcheck e2) of
        (Fail, _) -> Fail
        (_, Fail) -> Fail
        (Typed _ t1, Typed _ t2) -> case (t1, t2) of
          (TUnknown, t) -> Typed e t
          (t, TUnknown) -> Typed e t
          (a, b) -> if a == b then Typed e a else Fail
  e@(ExprLet bindings body) ->
    case typedcheck body of
      Typed _ t -> Typed e t
      Fail -> Fail

typecheck :: Expr -> A TypedExpression
typecheck expr = case expr of
  ExprLiteral (ValueInt n)     -> A $ Pair (TypedExprLiteral n) Int
  ExprLiteral (ValueDouble n)  -> A $ Pair (TypedExprLiteral n) Double
  ExprLiteral (ValueBool n)    -> A $ Pair (TypedExprLiteral n) Bool
  ExprLiteral (ValueString n)  -> A $ Pair (TypedExprLiteral n) String
  ExprVariable name            -> A $ Pair (TypedExprVariable name) Other
  ExprConstructor tag arity    -> A $ Pair (TypedExprConstructor tag arity) Constr
  ExprLambda vars body         -> typecheck body
  ExprApplication e1 e2        ->
    case (typecheck e1, typecheck e2) of
      (A (Pair x a), A (Pair y b)) -> A $ Pair (TypedExprApplication x y) a

  -- TODO: let
  -- ExprLet bindings body        ->
  --   case typecheck body of
  --     A (Pair x a) ->
  --       A $ Pair (TypedExprLet (map typecheckBinding bindings) x) a

-- typecheckBinding :: (Name, Expr) -> (Name, A TypedExpression)
-- typecheckBinding (name, expr) = (name, typecheck expr)
