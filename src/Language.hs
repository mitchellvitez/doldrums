module Language where

import Data.Text (Text, pack, intercalate)

-- | Expr is the data type of a Doldrums expression
data Expr                     -- ^ a is a binder, which is a name for a variable
  = ExprVariable Name         -- ^ variables
  | ExprNumber Int            -- ^ numbers
  -- TODO: add floating point numbers, strings, etc.
  -- | ExprFloat Double
  | ExprConstructor Int Int   -- ^ tag, arity
  | ExprApplication Expr Expr -- ^ apply first expr to the second
  | ExprLet                   -- ^ let expressions
      [(Name, Expr)]          -- ^ list of definitions
      Expr                    -- ^ body of the let expression
  | ExprCase                  -- ^ case expressions
      Expr                    -- ^ the expression under scrutiny
      [Alternative]           -- ^ list of alternatives
  | ExprLambda                -- ^ lambda expressions
      [Name]                  -- ^ list of lambda abstractions
      Expr                    -- ^ expression of the lambda
  deriving (Show, Eq)

type Name = Text

-- | Get the left hand side, or binders, of a list of definitions
lhsOf :: [(a, b)] -> [a]
lhsOf = map fst

rhsOf :: [(a, b)] -> [b]
rhsOf = map snd

-- tag, list of bound variables, expression to the right of the arrow
type Alternative = (Int, [Name], Expr)

-- | atomic expressions are expressions without any internal structure
isAtomicExpr :: Expr -> Bool
isAtomicExpr (ExprVariable v) = True
isAtomicExpr (ExprNumber n)   = True
isAtomicExpr _                = False

type Op = Text

-- a Doldrums program is a collection of supercombinators
type Program = [SupercombinatorDefinition]

type SupercombinatorDefinition =
  (Name, [Name], Expr) -- ^ name, list of arguments, body
