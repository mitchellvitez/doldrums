module Typecheck where

import Language
import Data.Text (Text)

data TypedExpr ty where
  TypedExprVariable :: Name -> TypedExpr ty
  TypedExprLiteral :: ty -> TypedExpr ty
  TypedExprLet :: [(Name, TypedExpr ty1)] -> TypedExpr ty
  TypedExprBinOp :: TypedBinOp tyIn tyOut -> TypedExpr tyIn -> TypedExpr tyIn -> TypedExpr tyOut
  TypedExprUnOp :: TypedUnOp tyIn tyOut -> TypedExpr tyIn -> TypedExpr tyOut
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
  | AddFloat -- +
  | SubFloat -- -
  | MulFloat -- *
  | DivFloat -- /
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
  Int :: Type Int
  Double :: Type Double
  String :: Type Text

data (f :*: g) x = f x :*: g x

type TypedExpression x = TypedExpr :*: Type
