module Interpret
  ( interpret
  )
where

import Language

import Control.Exception
import Control.Monad.State
import Data.Text
import Data.Map (Map)
import qualified Data.Map as Map

interpret :: Expr -> Text
interpret program = unpackExpr . fst $ runState (eval program) Map.empty

unpackExpr :: Expr -> Text
unpackExpr (ExprInt n) = tshow n
unpackExpr (ExprBool b) = tshow b
unpackExpr (ExprString s) = tshow s
unpackExpr (ExprDouble d) = tshow d
unpackExpr (ExprConstructor tag arity) = "Pack{" <> tshow tag <> ", " <> tshow arity <> "}"
unpackExpr x = "Incomplete evaluation: " <> tshow x

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

eval (ExprApplication (ExprVariable "~") a) = do
  evalA <- eval a
  case evalA of
    ExprInt x -> pure . ExprInt $ negate x
    ExprDouble x -> pure . ExprDouble $ negate x
    x -> throw . RuntimeException $ "Invalid argument to (~): " <> tshow x

eval (ExprApplication (ExprVariable "!") a) = do
  evalA <- eval a
  case evalA of
    ExprBool x -> pure . ExprBool $ not x
    x -> throw . RuntimeException $ "Invalid argument to (!): " <> tshow x

eval (ExprApplication (ExprApplication (ExprVariable op) a) b) = do
  env <- get
  case Map.lookup op env of
    Nothing -> case op of
      "+"  -> evalBinIntOp op a b
      "-"  -> evalBinIntOp op a b
      "*"  -> evalBinIntOp op a b
      "/"  -> evalBinIntOp op a b
      "+." -> evalBinDoubleOp op a b
      "-." -> evalBinDoubleOp op a b
      "*." -> evalBinDoubleOp op a b
      "/." -> evalBinDoubleOp op a b
      "||" -> evalBinBoolOp op a b
      "&&" -> evalBinBoolOp op a b
      "==" -> evalBinCompOp op a b
      "!=" -> evalBinCompOp op a b
      ">"  -> evalBinCompOp op a b
      ">=" -> evalBinCompOp op a b
      "<"  -> evalBinCompOp op a b
      "<=" -> evalBinCompOp op a b
      "$"  -> eval (ExprApplication a b)
      x -> throw . RuntimeException $ "Unknown operation: " <> x
    Just expr -> eval $ ExprApplication (ExprApplication expr a) b

-- basic lambda calculus
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
eval _ = error "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms"

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
      x -> throw . RuntimeException $ "Unknown binary integer operation: " <> x
  case (evalA, evalB) of
    (ExprInt x, ExprInt y) -> pure . ExprInt $ x `primOp` y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> op <> "): " <> tshow x <> ", " <> tshow y

evalBinDoubleOp :: Name -> Expr -> Expr -> State Env Expr
evalBinDoubleOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp = case op of
      "+." -> (+)
      "-." -> (-)
      "*." -> (*)
      "/." -> (/)
      x -> throw . RuntimeException $ "Unknown binary double operation: " <> x
  case (evalA, evalB) of
    (ExprDouble x, ExprDouble y) -> pure . ExprDouble $ x `primOp` y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> op <> "): " <> tshow x <> ", " <> tshow y

evalBinBoolOp :: Name -> Expr -> Expr -> State Env Expr
evalBinBoolOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp = case op of
      "&&" -> (&&)
      "||" -> (||)
      x -> throw . RuntimeException $ "Unknown binary bool operation: " <> x
  case (evalA, evalB) of
    (ExprBool x, ExprBool y) -> pure . ExprBool $ x `primOp` y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> op <> "): " <> tshow x <> ", " <> tshow y

evalBinCompOp :: Name -> Expr -> Expr -> State Env Expr
evalBinCompOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp = case op of
      "==" -> (==)
      "!=" -> (/=)
      "<"  -> (<)
      "<=" -> (<=)
      ">"  -> (>)
      ">=" -> (>=)
      x -> throw . RuntimeException $ "Unknown comparison operation: " <> x
  case (evalA, evalB) of
    (ExprInt x, ExprInt y) -> pure . ExprBool $ x `primOp` y
    -- TODO
    -- (ExprDouble x, ExprDouble y) -> pure . ExprBool $ x `primOp` y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> op <> "): " <> tshow x <> ", " <> tshow y
