module Language where

import Data.Text (Text, pack, intercalate)
-- import Data.Text.Lazy.Builder -- TODO

-- | Expr is the data type of a Doldrums expression
data Expr a                   -- ^ a is a binder, which is a name for a variable
  = ExprVariable Name         -- ^ variables
  | ExprNumber Int            -- ^ numbers
  -- TODO: add floating point numbers, strings, etc.
  -- | ExprFloat Double
  | ExprConstructor Int Int   -- ^ tag, arity -- TODO change tag to unique string
  | ExprApplication (Expr a) (Expr a)
  | ExprLet                   -- ^ let expressions
      [(a, Expr a)]           -- ^ list of definitions
      (Expr a)                -- ^ body of the let expression
  | ExprCase                  -- ^ case expressions
      (Expr a)                -- ^ the expression under scrutiny
      [Alternative a]         -- ^ list of alternatives
  | ExprLambda                -- ^ lambda expressions
      [a]                     -- ^ list of lambda abstractions
      (Expr a)                -- ^ expression of the lambda
  deriving (Show, Eq)

-- TODO: see if we can just use Name throughout
type CoreExpr = Expr Name

type Name = Text

-- | Get the left hand side, or binders, of a list of definitions
lhsOf :: [(a, b)] -> [a]
lhsOf = map fst

rhsOf :: [(a, b)] -> [b]
rhsOf = map snd

-- tag, list of bound variables, expression to the right of the arrow
type Alternative a = (Int, [a], Expr a)
type CoreAlternative = Alternative Name

-- | atomic expressions are expressions without any internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (ExprVariable v) = True
isAtomicExpr (ExprNumber n)   = True
isAtomicExpr _                = False

type Op = Text

-- a Doldrums program is a collection of supercombinators
type Program a = [SupercombinatorDefinition a]
type CoreProgram = Program Name

type SupercombinatorDefinition a =
  (Name, [a], Expr a) -- ^ name, list of arguments, body
type CoreSupercombinatorDefinition = SupercombinatorDefinition Name
