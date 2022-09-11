module Interpret where

import Language

import Control.Exception
import Control.Monad.State
import Data.Text
import Data.Map (Map)
import qualified Data.Map as Map

interpret :: Expr -> Text
interpret program = pack . show . fst $ runState (eval program) Map.empty

type Env = Map Name Expr

data RuntimeException = RuntimeException Text
  deriving (Eq, Show)
instance Exception RuntimeException

tshow :: Show a => a -> Text
tshow = pack . show

eval :: Expr -> State Env Expr
-- additional primitive operations
eval (ExprApplication (ExprApplication (ExprApplication (ExprVariable "if") pred) a) b) = do
  evalPred <- eval pred
  evalA <- eval a
  evalB <- eval b
  case evalPred of
    ExprBool True -> pure evalA
    ExprBool False -> pure evalB
    _ -> throw . RuntimeException $ "Invalid predicate in an if expression: " <> tshow pred

eval (ExprApplication (ExprApplication (ExprVariable "||") a) b) = do
  evalA <- eval a
  evalB <- eval b
  case (evalA, evalB) of
    (ExprBool x, ExprBool y) -> pure . ExprBool $ x || y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (||): " <> tshow x <> ", " <> tshow y

eval (ExprApplication (ExprApplication (ExprVariable "==") a) b) = do
  evalA <- eval a
  evalB <- eval b
  pure . ExprBool $ evalA == evalB

eval (ExprApplication (ExprApplication (ExprVariable "<") a) b) = do
  evalA <- eval a
  evalB <- eval b
  case (evalA, evalB) of
    (ExprInt x, ExprInt y) -> pure . ExprBool $ x < y
    (ExprDouble x, ExprDouble y) -> pure . ExprBool $ x < y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (<): " <> tshow x <> ", " <> tshow y

eval (ExprApplication (ExprApplication (ExprVariable op) a) b) = do
  env <- get
  case Map.lookup op env of
    Nothing -> evalBinIntOp op a b
    Just expr -> eval $ ExprApplication (ExprApplication expr a) b

-- eval :: Expr -> State Env Expr
-- lambda calculus
eval e@(ExprInt _) = pure e
eval e@(ExprBool _) = pure e
eval e@(ExprString _) = pure e
eval e@(ExprDouble _) = pure e
eval e@(ExprConstructor _ _) = pure e
eval e@(ExprLambda _ _) = pure e
eval (ExprVariable name) = do
  env <- get
  case Map.lookup name env of
    Nothing -> throw . RuntimeException $ "Unbound variable: " <> tshow name
    Just expr -> pure expr
eval (ExprLet name binding body) = do
  evalBinding <- eval binding
  modify $ Map.insert name evalBinding
  eval body
eval (ExprApplication func arg) = do
  evalFunc <- eval func
  case evalFunc of
    (ExprLambda name lambdaExpr) -> do
      evalArg <- eval arg
      modify $ Map.insert name evalArg
      eval lambdaExpr
    x -> throw . RuntimeException $ "Tried to apply something that isn't a lambda: " <> tshow x

evalBinIntOp :: Name -> Expr -> Expr -> State Env Expr
evalBinIntOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp = case op of
      "+" -> (+)
      "-" -> (-)
      "*" -> (*)
      "/" -> div
  case (evalA, evalB) of
    (ExprInt x, ExprInt y) -> pure . ExprInt $ x `primOp` y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> op <> "): " <> tshow x <> ", " <> tshow y
