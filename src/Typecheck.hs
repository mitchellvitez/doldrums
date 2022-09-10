{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Typecheck where

import Language

import Control.DeepSeq (NFData)
import Control.Exception
import GHC.Generics
import Data.Foldable (fold)
import Data.Kind hiding (Type)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec.Pos (SourcePos)

data SimpleType
  = Bool
  | Int
  | Float
  | String
  | Constr
  | SimpleType :-> SimpleType -- function type
  | Unknown
  deriving (Eq, Show, Generic, NFData)

data Typed e = Typed
  { expression :: e
  , exprType :: SimpleType
  }
  deriving (Eq, Show)

data TypeCheckingException = TypeCheckingException SourcePos String
  deriving (Eq, Show)
instance Exception TypeCheckingException

-- returns a list of top-level declarations and their types
-- throws an exception if there's a type mismatch
-- TODO: instead of checking a list of top-levels, this should check a tree starting with `main`
typecheck :: Program -> [(Name, SimpleType)]
typecheck = map (\(name, args, body) -> (name, exprType $ typecheckExpr args body))

typecheckArithmeticOperator :: String -> Expr -> Expr -> Expr -> SourcePos -> Annotation -> Typed Expr
typecheckArithmeticOperator name e a b pos annotation = case (typecheckExpr [] (Annotated a annotation), typecheckExpr [] (Annotated b annotation)) of
    (Typed _ Int    , Typed _ Int)     -> Typed e Int
    (Typed _ Int    , Typed _ Float)   -> Typed e Float
    (Typed _ Float  , Typed _ Int)     -> Typed e Float
    (Typed _ Float  , Typed _ Float)   -> Typed e Float
    (Typed _ Unknown, Typed _ Int)     -> Typed e Int
    (Typed _ Int    , Typed _ Unknown) -> Typed e Int
    (Typed _ Unknown, Typed _ Unknown) -> Typed e Unknown
    (Typed _ Unknown, Typed _ Float)   -> Typed e Float
    (Typed _ Float  , Typed _ Unknown) -> Typed e Float
    (Typed _ t1     , Typed _ Unknown) -> throw . TypeCheckingException pos $
      "Invalid first argument type for " <> name <> ": " <> show t1
    (Typed _ Unknown, Typed _ t2)      -> throw . TypeCheckingException pos $
      "Invalid second argument type for " <> name <> ": " <> show t2
    (Typed _ t1     , Typed _ t2)      -> throw . TypeCheckingException pos $
      "Invalid argument types for " <> name <> ": " <> show t1 <> ", " <> show t2

typecheckExpr :: [Name] -> Annotated Expr -> Typed Expr
typecheckExpr (x:xs) annot@(Annotated expr _) = Typed expr ((exprType $ typecheckExpr [] annot) :-> (exprType $ typecheckExpr xs annot))
typecheckExpr [] (Annotated expr annotation@(Annotation pos)) = case expr of
  e@(ExprLiteral (ValueInt n))    -> Typed e Int
  e@(ExprLiteral (ValueDouble n)) -> Typed e Float
  e@(ExprLiteral (ValueBool b))   -> Typed e Bool
  e@(ExprLiteral (ValueString s)) -> Typed e String

  e@(ExprApplication (ExprApplication (ExprVariable "+") a) b) -> typecheckArithmeticOperator "+" e a b pos annotation
  e@(ExprApplication (ExprApplication (ExprVariable "*") a) b) -> typecheckArithmeticOperator "*" e a b pos annotation
  e@(ExprApplication (ExprApplication (ExprVariable "/") a) b) -> typecheckArithmeticOperator "/" e a b pos annotation
  e@(ExprApplication (ExprApplication (ExprVariable "-") a) b) -> typecheckArithmeticOperator "-" e a b pos annotation

  -- TODO: if variable in list of known variables, use that type, otherwise assign a new TUnknown type to it
  e@(ExprVariable name)           -> Typed e Unknown
  e@(ExprConstructor tag arity)   -> Typed e Constr
  e@(ExprLambda vars body) ->
    case typecheckExpr [] (Annotated body annotation) of
      Typed _ t -> Typed e t
  e@(ExprApplication e1 e2) ->
    case e1 of
      ExprLiteral v -> throw $ TypeCheckingException pos $ "Trying to apply a literal: " <> show v
      ExprConstructor _ _ -> throw $ TypeCheckingException pos "Trying to apply a constructor"
      _ -> case (typecheckExpr [] (Annotated e1 annotation), typecheckExpr [] (Annotated e2 annotation)) of
        (Typed _ t1, Typed _ t2) -> case (t1, t2) of
          (Unknown, t) -> Typed e t
          (t, Unknown) -> Typed e t
          (a, b) -> if a == b
            then Typed e a
            else throw $ TypeCheckingException pos $ show a <> " does not match " <> show b
  e@(ExprLet bindings body) ->
    case typecheckExpr [] (Annotated body annotation) of
      Typed _ t -> Typed e t
