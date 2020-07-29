module Language where

import Data.Text (Text, pack, intercalate)

-- a Doldrums program is a collection of supercombinators
type Program = [SupercombinatorDefinition]

type SupercombinatorDefinition =
  (Name, [Name], Expr) -- ^ name, list of arguments, body

type Name = Text

data Expr
  = ExprVariable Name
  | ExprLiteral Value
  | ExprConstructor Int Int -- tag, arity
  | ExprApplication Expr Expr
  | ExprLet [(Name, Expr)] Expr -- definitions, body
  | ExprLambda [Name] Expr
  deriving (Show, Eq)

data Value
  = ValueInt Integer
  | ValueDouble Double
  | ValueBool Bool
  | ValueString Text
  deriving (Show, Eq, Ord)
