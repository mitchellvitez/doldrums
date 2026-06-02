{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Interpret
  ( interpret
  )
where

import Language

import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Exception
import Control.Monad (forM, when)
import Control.Monad.State
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- | variable names map to thunks
type Env = Map Name ThunkId

-- | track the heap of all thunks by id, and the next available id, alongside current env
data EvalState = EvalState
  { heap :: IntMap ThunkInfo
  , nextId :: ThunkId
  , env :: Env
  }

-- | evaluated form of Expr
data Value
  = ValLiteral Literal
  -- | data constructor, partially applied unless Arity is 0
  | ValConstructor Tag Arity
  -- | function closure (i.e. a lambda), Name is the bound argument, Expr is the body
  | ValClosure Name Expr Env
  -- | partially applied constructor or function, Value is function/constructor being applied, ThunkId is the argument
  | ValApply Value ThunkId

-- | a thunk is tracked by id
newtype ThunkId = ThunkId { unThunkId :: Int }
  deriving newtype (Show, Enum)

data ThunkInfo
  -- | unevaluated expression with the env it was created in
  = Thunk Env Expr
  -- | evaluation in progress, hitting this during eval means a cycle
  | BlackHole
  -- | evaluated, cached value
  | Done Value

data RuntimeException = RuntimeException Text
  deriving (Eq, Show)
instance Exception RuntimeException

tshow :: Show a => a -> Text
tshow = T.pack . show

instance Show Value where
  show (ValLiteral l) = show l
  show (ValConstructor tag _) = show tag
  show (ValClosure arg _ _) = "<function arg=" <> T.unpack (unName arg) <> ">"
  show (ValApply _ t) = "<thunk id=" <> show t <> ">"

-- | takes top-level bindings and the `main` Expr, evaluates to user-facing Text result
interpret :: [(Name, Expr)] -> Expr -> Text
interpret bindings mainExpr =
  fst $ flip runState initialState $ do
    -- evaluate `main` Expr to a Value
    val <- withEnv topLevelEnv $ whnf mainExpr
    -- convert that Value to Text for display
    unpackValue val
 where
  -- allocate a ThunkId for every top-level binding before evaluating
  -- so all names are in scope for all bindings (mutual recursion)
  tids = map ThunkId [0 .. length bindings - 1]
  topLevelEnv = Map.fromList $ zip (map fst bindings) tids
  initialState =
    EvalState
      { heap =
          IntMap.fromList
            [ (unThunkId tid, Thunk topLevelEnv expr)
            | (tid, (_, expr)) <- zip tids bindings
            ]
      , nextId = ThunkId $ length bindings
      , env = Map.empty
      }

-- | allocate a thunk capturing the current env
newThunkInCurrentEnv :: Expr -> State EvalState ThunkId
newThunkInCurrentEnv expr = do
  s <- get
  let tid = nextId s
  put s
    { heap = IntMap.insert (unThunkId tid) (Thunk (env s) expr) (heap s)
    , nextId = ThunkId $ unThunkId tid + 1
    }
  pure tid

-- | allocate a thunk with an explicit env
newThunkWithEnv :: Env -> Expr -> State EvalState ThunkId
newThunkWithEnv e expr = do
  s <- get
  let tid = nextId s
  put s
    { heap = IntMap.insert (unThunkId tid) (Thunk e expr) (heap s)
    , nextId = ThunkId $ unThunkId tid + 1
    }
  pure tid

force :: ThunkId -> State EvalState Value
force tid = do
  state <- get
  case IntMap.lookup (unThunkId tid) (heap state) of
    Nothing -> throw $ RuntimeException $ "Unknown thunk, id = " <> tshow tid
    Just (Done v) -> pure v
    Just BlackHole -> throw $ RuntimeException "Infinite loop detected"
    Just (Thunk savedEnv expr) -> do
      -- mark as BlackHole
      modify $ \s -> s { heap = IntMap.insert (unThunkId tid) BlackHole (heap s) }
      -- evaluate with the env that was captured when the thunk was created
      val <- withEnv savedEnv $ whnf expr
      -- then mark as Done
      modify $ \s -> s { heap = IntMap.insert (unThunkId tid) (Done val) (heap s) }
      pure val

-- | run an action with a temporary env, restoring the original afterward
withEnv :: Env -> State EvalState a -> State EvalState a
withEnv newEnv action = do
  oldEnv <- gets env
  modify $ \s -> s { env = newEnv }
  result <- action
  modify $ \s -> s { env = oldEnv }
  pure result

-- | evaluate an Expr to weak head normal form. this is as far as `force` goes
whnf :: Expr -> State EvalState Value
whnf (ExprLiteral l) = pure $ ValLiteral l
whnf (ExprConstructor tag arity) = pure $ ValConstructor tag arity
whnf (ExprLambda name body) = do
  -- capture the current env in the closure
  currentEnv <- gets env
  pure $ ValClosure name body currentEnv
whnf (ExprVariable name) = do
  currentEnv <- gets env
  case Map.lookup name currentEnv of
    Just tid -> force tid
    Nothing  -> throw $ RuntimeException $ "Unbound variable: " <> tshow name
whnf (ExprLet bindings body) = do
  s <- get
  let
    -- to allow mutual recursion, write out all bindings as thunks
    names = map fst bindings
    exprs = map snd bindings
    baseTid = unThunkId $ nextId s
    tids = map ThunkId [baseTid .. baseTid + length bindings - 1]
    recEnv = foldr (\(n, tid) e -> Map.insert n tid e) (env s) $ zip names tids
    newHeap = foldr
      (\(tid, expr) h -> IntMap.insert (unThunkId tid) (Thunk recEnv expr) h)
      (heap s)
      (zip tids exprs)
  put s
    { heap = newHeap
    , nextId = ThunkId $ baseTid + length bindings
    , env = recEnv
    }
  whnf body
-- binary operations
whnf (ExprApplication (ExprApplication (ExprVariable (Name op)) a) b) =
  case op of
    -- primitive ops
    "<>" -> strBinOp a b
    "+"  -> intBinOp (+) a b
    "-"  -> intBinOp (-) a b
    "*"  -> intBinOp (*) a b
    "/"  -> intBinOp div a b
    "+." -> doubleBinOp (+) a b
    "-." -> doubleBinOp (-) a b
    "*." -> doubleBinOp (*) a b
    "/." -> doubleBinOp (/) a b
    -- comparision operators (==, !=, <, >, <=, >=) are desugared into a `case` on the `compare` primitive
    "compare" -> comparePrim a b
    -- && and || are already desugared into `case` expressions
    _    -> do
      currentEnv <- gets env
      case Map.lookup (Name op) currentEnv of
        Just tid -> do
          funcVal <- force tid
          afterA <- applyValue funcVal a
          applyValue afterA b
        Nothing -> throw $ RuntimeException $ "Unknown operation: " <> op
-- unary operations
whnf (ExprApplication (ExprVariable (Name op)) arg)
  | op == "~" = unaryNeg arg
  | op == "!" = unaryNot arg
  | op == "show" = showPrim arg
  | otherwise    = do
      currentEnv <- gets env
      case Map.lookup (Name op) currentEnv of
        Just tid -> do
          funcVal <- force tid
          applyValue funcVal arg
        Nothing  -> throw $ RuntimeException $ "Unbound variable: " <> op
-- function applications
whnf (ExprApplication func arg) = do
  funcVal <- whnf func
  applyValue funcVal arg
-- case expressions
whnf (ExprCase scrutinee alts) = do
  scrutVal <- whnf scrutinee
  case unApply scrutVal [] of
    Right (tag, arity, argThunks) -> do
      let
        matchingAlt = find (\(Alternative pat _) -> case pat of
          PatternConstructor altTag _ -> altTag == tag
          _ -> False) alts
        Alternative (PatternConstructor _ argPats) altBody =
          fromMaybe (throw $ RuntimeException $ "Non-exhaustive patterns for tag: " <> unTag tag) $ matchingAlt
      when (length argThunks /= unArity arity) $
        throw $ RuntimeException $ "Constructor not fully applied: " <> unTag tag
      when (length argPats /= length argThunks) $
        throw $ RuntimeException $ "Pattern arity mismatch for constructor: " <> unTag tag
      let argNames = concatMap patternNames argPats
      -- bind pattern variables to the constructor's argument thunks
      currentEnv <- gets env
      let patternEnv = foldr (\(n, t) e -> Map.insert n t e) currentEnv (zip argNames argThunks)
      withEnv patternEnv $ whnf altBody
    Left _ -> do
      let
        matchingAlt = find (\(Alternative pat _) -> case pat of
          PatternLiteral patLit -> case scrutVal of
            ValLiteral valLit -> patLit == valLit
            _ -> False
          PatternVar _ -> True
          PatternWildcard -> True
          _ -> False) alts
      case matchingAlt of
        Nothing -> throw $ RuntimeException "Non-exhaustive patterns"
        Just (Alternative pat altBody) -> do
          currentEnv <- gets env
          patternEnv <- case pat of
            PatternVar n -> do
              tid <- newThunkInCurrentEnv scrutinee
              pure $ Map.insert n tid currentEnv
            _ -> pure currentEnv
          withEnv patternEnv $ whnf altBody
-- the above should be exhaustive
whnf x = error $ "Failed pattern match exhaustiveness: " <> show x

-- applies a Value to a single Expr argument (always single due to currying)
applyValue :: Value -> Expr -> State EvalState Value
applyValue (ValClosure name body closureEnv) arg = do
  -- allocate the arg thunk in the current env (where arg is evaluated)
  currentEnv <- gets env
  argThunk <- newThunkWithEnv currentEnv arg
  -- evaluate the body in the closure's captured env, extended with the new arg binding
  let bodyEnv = Map.insert name argThunk closureEnv
  withEnv bodyEnv $ whnf body
applyValue (ValConstructor tag arity) arg = do
  currentEnv <- gets env
  argThunk <- newThunkWithEnv currentEnv arg
  pure $ ValApply (ValConstructor tag arity) argThunk
applyValue (ValApply v t) arg = do
  currentEnv <- gets env
  argThunk <- newThunkWithEnv currentEnv arg
  pure $ ValApply (ValApply v t) argThunk
applyValue v _ = throw $ RuntimeException $ "Tried to apply something that isn't a function: " <> tshow v

unApply :: Value -> [ThunkId] -> Either Text (Tag, Arity, [ThunkId])
unApply (ValConstructor tag arity) acc = Right (tag, arity, acc)
unApply (ValApply val tid) acc = unApply val (tid:acc)
unApply _ _ = Left "Not a constructor"

intBinOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> State EvalState Value
intBinOp op a b = do
  valA <- whnf a
  valB <- whnf b
  case (valA, valB) of
    (ValLiteral (LiteralInt rawA), ValLiteral (LiteralInt rawB)) ->
      pure . ValLiteral . LiteralInt $ rawA `op` rawB
    _ -> throw $ RuntimeException "Type error in binary Integer operation"

doubleBinOp :: (Double -> Double -> Double) -> Expr -> Expr -> State EvalState Value
doubleBinOp op a b = do
  valA <- whnf a
  valB <- whnf b
  case (valA, valB) of
    (ValLiteral (LiteralFloat rawA), ValLiteral (LiteralFloat rawB)) ->
      pure . ValLiteral . LiteralFloat $ rawA `op` rawB
    _ -> throw $ RuntimeException "Type error in binary Double operation"

strBinOp :: Expr -> Expr -> State EvalState Value
strBinOp a b = do
  valA <- whnf a
  valB <- whnf b
  case (valA, valB) of
    (ValLiteral (LiteralString rawA), ValLiteral (LiteralString rawB)) ->
      pure . ValLiteral . LiteralString $ rawA <> rawB
    _ -> throw $ RuntimeException "Type error in string concatenation"

comparePrim :: Expr -> Expr -> State EvalState Value
comparePrim a b = do
  valA <- whnf a
  valB <- whnf b
  case (valA, valB) of
    (ValLiteral (LiteralInt rawA), ValLiteral (LiteralInt rawB)) -> do
      let result = compare rawA rawB
      pure $ ValConstructor (Tag $ tshow result) (Arity 0)
    (ValLiteral (LiteralFloat rawA), ValLiteral (LiteralFloat rawB)) -> do
      let result = compare rawA rawB
      pure $ ValConstructor (Tag $ tshow result) (Arity 0)
    (ValLiteral (LiteralString rawA), ValLiteral (LiteralString rawB)) -> do
      let result = compare rawA rawB
      pure $ ValConstructor (Tag $ tshow result) (Arity 0)
    _ -> throw $ RuntimeException "Invalid argument to compare"

unaryNeg :: Expr -> State EvalState Value
unaryNeg a = do
  valA <- whnf a
  case valA of
    ValLiteral (LiteralInt rawA) -> pure . ValLiteral . LiteralInt $ negate rawA
    ValLiteral (LiteralFloat rawA) -> pure . ValLiteral . LiteralFloat $ negate rawA
    _ -> throw $ RuntimeException "Invalid argument to (~)"

unaryNot :: Expr -> State EvalState Value
unaryNot a = do
  valA <- whnf a
  case valA of
    ValConstructor (Tag "True") _ -> pure $ ValConstructor (Tag "False") $ Arity 0
    ValConstructor (Tag "False") _ -> pure $ ValConstructor (Tag "True") $ Arity 0
    _ -> throw $ RuntimeException "Invalid argument to (!)"

showPrim :: Expr -> State EvalState Value
showPrim a = do
  val <- whnf a
  str <- unpackValue val
  pure . ValLiteral $ LiteralString str

unpackValue :: Value -> State EvalState Text
unpackValue (ValLiteral (LiteralInt n)) = pure $ tshow n
unpackValue (ValLiteral (LiteralString s)) = pure s
unpackValue (ValLiteral (LiteralFloat f)) = pure $ tshow f
unpackValue (ValConstructor tag (Arity 0)) = pure $ unTag tag
unpackValue (ValConstructor tag _) = pure $ "<partially applied " <> unTag tag <> ">"
unpackValue (ValClosure _ _ _) = pure "<function>"
unpackValue v = case unApply v [] of
  Right (tag, _, args) -> do
    argsText <- forM args $ \tid -> do
      val <- force tid
      innerText <- unpackValue val
      pure $ if T.any (== ' ') innerText then "(" <> innerText <> ")" else innerText
    pure $ T.unwords (unTag tag : argsText)
  Left _ -> pure $ "Incomplete evaluation: " <> tshow v
