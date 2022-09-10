module Typecheck where

import Language

import Control.Exception
import Data.Foldable (fold)
import Data.Kind hiding (Type)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec.Pos (sourcePosPretty)

data SimpleType
  = TBool
  | TInt
  | TDouble
  | TString
  | TConstr
  | TUnknown -- defer typechecking until we can infer from somewhere else
  deriving (Eq, Show)

data Typed e = Typed e SimpleType
  deriving (Eq, Show)

data TypeCheckingException = TypeCheckingException String
  deriving (Eq, Show)
instance Exception TypeCheckingException

typecheck :: Program -> [Typed Expr]
typecheck = map (\(_name, _args, body) -> typecheckExpr body)

typecheckExpr :: Annotated Expr -> Typed Expr
typecheckExpr (Annotated expr annotation@(Annotation pos)) = case expr of
  e@(ExprLiteral (ValueInt n))    -> Typed e TInt
  e@(ExprLiteral (ValueDouble n)) -> Typed e TDouble
  e@(ExprLiteral (ValueBool b))   -> Typed e TBool
  e@(ExprLiteral (ValueString s)) -> Typed e TString
  e@(ExprVariable name)           -> Typed e TUnknown
  e@(ExprConstructor tag arity)   -> Typed e TConstr
  e@(ExprLambda vars body) ->
    case typecheckExpr (Annotated body annotation) of
      Typed _ t -> Typed e t
  e@(ExprApplication e1 e2) ->
    case e1 of
      ExprLiteral _ -> throw $ TypeCheckingException "Trying to apply a literal"
      ExprConstructor _ _ -> throw $ TypeCheckingException "Trying to apply a constructor"
      _ -> case (typecheckExpr (Annotated e1 annotation), typecheckExpr (Annotated e2 annotation)) of
        (Typed _ t1, Typed _ t2) -> case (t1, t2) of
          (TUnknown, t) -> Typed e t
          (t, TUnknown) -> Typed e t
          (a, b) -> if a == b then Typed e a else (throw $ TypeCheckingException $ fold
            [ "Type mismatch at "
            , sourcePosPretty pos
            , ", "
            , show a
            , " does not match "
            , show b
            ])
  e@(ExprLet bindings body) ->
    case typecheckExpr (Annotated body annotation) of
      Typed _ t -> Typed e t
