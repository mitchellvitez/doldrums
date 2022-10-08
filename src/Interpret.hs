{-# LANGUAGE PatternSynonyms #-}

module Interpret
  ( interpret
  )
where

import Language

import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Exception
import Control.Monad.State
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

interpret :: Expr -> Text
interpret program = unpackExpr . fst $ runState (eval program) Map.empty

unpackExpr :: Expr -> Text
unpackExpr (ExprInt n) = tshow n
unpackExpr (ExprString s) = tshow s
unpackExpr (ExprDouble d) = tshow d
unpackExpr (ExprConstructor tag arity) = tshow tag
unpackExpr x = case tagAndArgs x [] of
  Just (tag, args) -> "(" <> unTag tag <> " " <> T.intercalate " " (map unpackExpr args) <> ")"
  Nothing -> "Incomplete evaluation: " <> tshow x
  where
    tagAndArgs (ExprApplication expr arg) acc = tagAndArgs expr (arg:acc)
    tagAndArgs e@(ExprConstructor tag (Arity arity)) acc | length acc == arity = Just (tag, acc)
    tagAndArgs _ _ = Nothing

type Env = Map Name Expr

data RuntimeException = RuntimeException Text
  deriving (Eq, Show)
instance Exception RuntimeException

tshow :: Show a => a -> Text
tshow = T.pack . show

pattern App1 expr a       = ExprApplication expr a
pattern App2 expr a b     = ExprApplication (ExprApplication expr a) b
pattern App3 expr a b c   = ExprApplication (ExprApplication (ExprApplication expr a) b) c
pattern App4 expr a b c d = ExprApplication (ExprApplication (ExprApplication (ExprApplication expr a) b) c) d

eval :: Expr -> State Env Expr
-- additional primitive operations
eval (App3 (ExprVariable "if") pred a b) = do
  evalPred <- eval pred
  evalA <- eval a
  evalB <- eval b
  case evalPred of
    ExprConstructor (Tag "True") (Arity 0) -> pure evalA
    ExprConstructor (Tag "False") (Arity 0) -> pure evalB
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
    ExprConstructor (Tag "True") (Arity 0) -> pure $ ExprConstructor (Tag "False") (Arity 0)
    ExprConstructor (Tag "False") (Arity 0) -> pure $ ExprConstructor (Tag "True") (Arity 0)
    x -> throw . RuntimeException $ "Invalid argument to (!): " <> tshow x

eval (App2 (ExprVariable op) a b) = do
  env <- get
  case Map.lookup op env of
    Nothing -> case op of
      Name "+"  -> evalBinIntOp op a b
      Name "-"  -> evalBinIntOp op a b
      Name "*"  -> evalBinIntOp op a b
      Name "/"  -> evalBinIntOp op a b
      Name "+." -> evalBinDoubleOp op a b
      Name "-." -> evalBinDoubleOp op a b
      Name "*." -> evalBinDoubleOp op a b
      Name "/." -> evalBinDoubleOp op a b
      Name "||" -> evalBinBoolOp op a b
      Name "&&" -> evalBinBoolOp op a b
      Name "==" -> evalBinCompOp op a b
      Name "!=" -> evalBinCompOp op a b
      Name ">"  -> evalBinCompOp op a b
      Name ">=" -> evalBinCompOp op a b
      Name "<"  -> evalBinCompOp op a b
      Name "<=" -> evalBinCompOp op a b
      Name "$"  -> eval (ExprApplication a b)
      x -> throw . RuntimeException $ "Unknown operation: " <> unName x
    Just expr -> eval $ App2 expr a b

-- basic lambda calculus
eval e@(ExprInt _) = pure e
eval e@(ExprString _) = pure e
eval e@(ExprDouble _) = pure e
eval e@(ExprConstructor _ _) = pure e
eval e@(ExprLambda _ _) = pure e
eval (ExprVariable name) = do
  env <- get
  case Map.lookup name env of
    Nothing -> throw . RuntimeException $ "Unbound variable: " <> tshow name
    Just expr -> eval expr
eval (ExprLet name binding body) = do
  modify $ Map.insert name binding
  eval body
eval e@(ExprApplication func arg) = do
  evalFunc <- eval func
  case evalFunc of
    (ExprLambda name lambdaExpr) -> do
      evalArg <- eval arg
      modify $ Map.insert name evalArg
      eval lambdaExpr
    (ExprConstructor _ _) -> pure e
    (App1 (ExprConstructor _ _) _) -> pure e
    (App2 (ExprConstructor _ _) _ _) -> pure e
    (App3 (ExprConstructor _ _) _ _ _) -> pure e
    (App4 (ExprConstructor _ _) _ _ _ _) -> pure e
    x -> throw . RuntimeException $ "Tried to apply something that isn't a lambda: " <> tshow x

eval (ExprCase scrutinee alts) = do
  evalScrutinee <- eval scrutinee
  let (tag, args) = tagAndArgs evalScrutinee []
      Alternative _ argNames altBody = findOrErr ((==tag) . altTag) alts
  mapM_ (modify . uncurry Map.insert) $ zip argNames args
  eval altBody

  where
    tagAndArgs (ExprApplication expr arg) acc = tagAndArgs expr (arg:acc)
    tagAndArgs e@(ExprConstructor tag (Arity arity)) acc | length acc == arity = (tag, acc)
    tagAndArgs e _ = throw err

    findOrErr finder = fromMaybe (throw err) . find finder

    err = RuntimeException $ "Couldn't match expression: " <> tshow scrutinee

    altTag (Alternative altTag _ _) = altTag

eval x = error $ "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms: " <> show x


evalBinIntOp :: Name -> Expr -> Expr -> State Env Expr
evalBinIntOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp = case op of
      Name "+" -> (+)
      Name "-" -> (-)
      Name "*" -> (*)
      Name "/" -> div
      x -> throw . RuntimeException $ "Unknown binary integer operation: " <> unName x
  case (evalA, evalB) of
    (ExprInt x, ExprInt y) -> pure . ExprInt $ x `primOp` y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> unName op <> "): " <> tshow x <> ", " <> tshow y

evalBinDoubleOp :: Name -> Expr -> Expr -> State Env Expr
evalBinDoubleOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp = case op of
      Name "+." -> (+)
      Name "-." -> (-)
      Name "*." -> (*)
      Name "/." -> (/)
      x -> throw . RuntimeException $ "Unknown binary double operation: " <> unName x
  case (evalA, evalB) of
    (ExprDouble x, ExprDouble y) -> pure . ExprDouble $ x `primOp` y
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> unName op <> "): " <> tshow x <> ", " <> tshow y

evalBinBoolOp :: Name -> Expr -> Expr -> State Env Expr
evalBinBoolOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp = case op of
      Name "&&" -> (&&)
      Name "||" -> (||)
      x -> throw . RuntimeException $ "Unknown binary bool operation: " <> unName x
  case (evalA, evalB) of
    (ExprConstructor (Tag x) (Arity 0), ExprConstructor (Tag y) (Arity 0)) -> pure $ ExprConstructor (Tag (tshow $ toBool x `primOp` toBool y)) (Arity 0)
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> unName op <> "): " <> tshow x <> ", " <> tshow y
  where toBool "True" = True
        toBool "False" = False
        toBool _ = error "evalBinBoolOp"

evalBinCompOp :: Name -> Expr -> Expr -> State Env Expr
evalBinCompOp op a b = do
  evalA <- eval a
  evalB <- eval b
  let
    primOp :: Ord a => a -> a -> Bool
    primOp = case op of
      Name "==" -> (==)
      Name "!=" -> (/=)
      Name "<"  -> (<)
      Name "<=" -> (<=)
      Name ">"  -> (>)
      Name ">=" -> (>=)
      x -> throw . RuntimeException $ "Unknown comparison operation: " <> unName x
  case (evalA, evalB) of
    (ExprInt x, ExprInt y) -> pure $ ExprConstructor (Tag (tshow (x `primOp` y))) (Arity 0)
    (ExprDouble x, ExprDouble y) -> pure $ ExprConstructor (Tag (tshow (x `primOp` y))) (Arity 0)
    (x, y) -> throw . RuntimeException $ "Invalid arguments to (" <> unName op <> "): " <> tshow x <> ", " <> tshow y
