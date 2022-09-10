module Language where

import Data.Text (Text, pack, intercalate)
import Text.Megaparsec (SourcePos)

-- a Doldrums program is a collection of supercombinators
type Program = [SupercombinatorDefinition]

type Name = Text

type SupercombinatorDefinition =
  (Name, [Name], Annotated Expr) -- ^ name, list of arguments, body

data Annotation = Annotation { sourcePos :: SourcePos }
  deriving (Eq, Show)

data Annotated e = Annotated e Annotation
  deriving (Eq, Show)

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
