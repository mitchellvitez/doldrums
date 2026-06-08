{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Interpret
  ( interpret
  , methodEnvFromInstances
  )
where

import Language

import Data.List (find)
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

-- method dispatch environment: method name -> (type tag -> instance body)
-- runtime type-based dispatch of typeclass methods.
-- type tags are "Int", "Double", "String", or DataType names (e.g. "Bool", "Maybe")
type MethodEnv = Map Name (Map Name Expr)

-- | map from constructor tags to their data type names
type DataTypeMap = Map Tag Name

-- | build a MethodEnv from instance declarations
methodEnvFromInstances :: [InstanceDeclaration ()] -> DataTypeMap -> MethodEnv
methodEnvFromInstances instances typeMap =
  Map.fromListWith Map.union
    [ (methodName, Map.singleton typeTag body)
    | InstanceDeclaration _ _ instTypes meths <- instances
    , (methodName, body) <- meths
    , ty <- instTypes
    , typeTag <- typeHintToTags ty typeMap
    ]

-- | convert a TypeHint to the type tags used for dispatch
typeHintToTags :: TypeHint -> DataTypeMap -> [Name]
typeHintToTags TypeHintInt _ = [Name "Int"]
typeHintToTags TypeHintDouble _ = [Name "Double"]
typeHintToTags TypeHintString _ = [Name "String"]
typeHintToTags (TypeHintApp dt _) _ = [Name $ unDataType dt]
typeHintToTags (TypeHintConstructor dt) _ = [Name $ unDataType dt]
typeHintToTags (TypeHintVar _) _ = []
typeHintToTags _ _ = []

-- | get the type name tag from an evaluated value for method dispatch
valueTypeTag :: Value -> DataTypeMap -> Name
valueTypeTag (ValLiteral (LiteralInt _)) _ = Name "Int"
valueTypeTag (ValLiteral (LiteralDouble _)) _ = Name "Double"
valueTypeTag (ValLiteral (LiteralString _)) _ = Name "String"
valueTypeTag (ValConstructor tag _) typeMap =
  case Map.lookup tag typeMap of
    Just n -> n
    Nothing -> Name $ unTag tag
valueTypeTag (ValApply v _) typeMap = valueTypeTag v typeMap
valueTypeTag _ _ = Name "<unknown>"

-- | track the heap of all thunks by id, and the next available id, alongside current env
data EvalState = EvalState
  { heap :: IntMap ThunkInfo
  , nextId :: ThunkId
  , env :: Env
  , methodEnv :: MethodEnv
  , typeMap :: DataTypeMap
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
  -- | pick the right typeclass method from method name and type name
  | ValMethodDispatch Name (Map Name Expr)

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
  show (ValMethodDispatch name _) = "<method name=" <> T.unpack (unName name) <> ">"

-- | takes top-level bindings, method dispatch env, data type map, and the `main` Expr
interpret :: [(Name, Expr)] -> MethodEnv -> DataTypeMap -> Expr -> Text
interpret bindings methodEnv typeMap mainExpr =
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
      , methodEnv = methodEnv
      , typeMap = typeMap
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
  curState <- get
  case IntMap.lookup (unThunkId tid) (heap curState) of
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
  -- check if this is a typeclass method name first
  mMethod <- gets (Map.lookup name . methodEnv)
  case mMethod of
    Just dispatchTable -> do
      pure $ ValMethodDispatch name dispatchTable
    Nothing -> do
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
    -- primitive ops (typeclass-aware dispatch)
    "<>" -> strBinOp a b
    "+"  -> numBinOp (+) (+) a b
    "-"  -> numBinOp (-) (-) a b
    "*"  -> numBinOp (*) (*) a b
    "/"  -> numBinOp (div) (/) a b
    -- comparision operators (==, /=, <, >, <=, >=) are desugared into a `case` on the `compare` primitive
    "compare" -> comparePrim a b
    -- && and || are already desugared into `case` expressions
    _    -> do
      mMethod <- gets (Map.lookup (Name op) . methodEnv)
      case mMethod of
        Just dispatchTable -> do
          afterA <- applyValue (ValMethodDispatch (Name op) dispatchTable) a
          applyValue afterA b
        Nothing -> do
          currentEnv <- gets env
          case Map.lookup (Name op) currentEnv of
            Just tid -> do
              funcVal <- force tid
              afterA <- applyValue funcVal a
              applyValue afterA b
            Nothing -> throw $ RuntimeException $ "Unknown operation: " <> op
-- unary operations
whnf (ExprApplication (ExprVariable (Name op)) arg)
  | op == "show" = showPrim arg
  | op == "words" = wordsPrim arg
  | op == "lines" = linesPrim arg
  | op == "unwords" = unwordsPrim arg
  | op == "unlines" = unlinesPrim arg
  | op == "floor" = floorPrim arg
  | op == "ceiling" = ceilingPrim arg
  | op == "round" = roundPrim arg
  | otherwise    = do
      mMethod <- gets (Map.lookup (Name op) . methodEnv)
      case mMethod of
        Just dispatchTable ->
          applyValue (ValMethodDispatch (Name op) dispatchTable) arg
        Nothing -> do
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
          PatternVar _ -> True
          PatternWildcard -> True
          _ -> False) alts
        altList = [unTag t | Alternative (PatternConstructor t _) _ <- alts]
      case matchingAlt of
        Just (Alternative (PatternConstructor _ argPats) altBody) -> do
          when (length argThunks /= unArity arity) $
            throw $ RuntimeException $ "Constructor not fully applied: " <> unTag tag
          when (length argPats /= length argThunks) $
            throw $ RuntimeException $ "Pattern arity mismatch for constructor: " <> unTag tag
          let argNames = concatMap patternNames argPats
          currentEnv <- gets env
          let patternEnv = foldr (\(n, t) e -> Map.insert n t e) currentEnv (zip argNames argThunks)
          withEnv patternEnv $ whnf altBody
        Just (Alternative (PatternVar n) altBody) -> do
          currentEnv <- gets env
          tid <- newThunkInCurrentEnv scrutinee
          let patternEnv = Map.insert n tid currentEnv
          withEnv patternEnv $ whnf altBody
        Just (Alternative PatternWildcard altBody) -> do
          whnf altBody
        Nothing ->
          throw $ RuntimeException $ T.concat ["Non-exhaustive patterns for tag: ", unTag tag, " (available: ", T.intercalate ", " altList, ")"]
        _ -> throw $ RuntimeException "Unexpected pattern type"
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
applyValue (ValMethodDispatch name dispatchTable) arg = do
  argVal <- whnf arg
  tmap <- gets typeMap
  let tag = valueTypeTag argVal tmap
  case Map.lookup tag dispatchTable of
    Just body -> do
      bodyVal <- whnf body
      applyValue bodyVal arg
    Nothing  -> do
      throw $ RuntimeException $
        "No matching instance for method " <> unName name
        <> " for type " <> unName tag
applyValue v _ = throw $ RuntimeException $ "Tried to apply something that isn't a function: " <> tshow v

unApply :: Value -> [ThunkId] -> Either Text (Tag, Arity, [ThunkId])
unApply (ValConstructor tag arity) acc = Right (tag, arity, acc)
unApply (ValApply val tid) acc = unApply val (tid:acc)
unApply _ _ = Left "Not a constructor"

numBinOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Expr -> Expr -> State EvalState Value
numBinOp intOp doubleOp a b = do
  valA <- whnf a
  case valA of
    ValLiteral (LiteralInt rawA) -> do
      valB <- whnf b
      case valB of
        ValLiteral (LiteralInt rawB) -> pure . ValLiteral . LiteralInt $ intOp rawA rawB
        _ -> throw $ RuntimeException "Type error in numeric operation"
    ValLiteral (LiteralDouble rawA) -> do
      valB <- whnf b
      case valB of
        ValLiteral (LiteralDouble rawB) -> pure . ValLiteral . LiteralDouble $ doubleOp rawA rawB
        _ -> throw $ RuntimeException "Type error in numeric operation"
    _ -> throw $ RuntimeException "Type error in numeric operation"

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
    (ValLiteral (LiteralDouble rawA), ValLiteral (LiteralDouble rawB)) -> do
      let result = compare rawA rawB
      pure $ ValConstructor (Tag $ tshow result) (Arity 0)
    (ValLiteral (LiteralString rawA), ValLiteral (LiteralString rawB)) -> do
      let result = compare rawA rawB
      pure $ ValConstructor (Tag $ tshow result) (Arity 0)
    _ -> throw $ RuntimeException "Invalid argument to compare"

showPrim :: Expr -> State EvalState Value
showPrim a = do
  val <- whnf a
  str <- unpackValue val
  pure . ValLiteral $ LiteralString str

buildStringListExpr :: [Text] -> Expr
buildStringListExpr = foldr (\s acc ->
  ExprApplication
    (ExprApplication
      (ExprConstructor (Tag "Cons") (Arity 2))
      (ExprLiteral (LiteralString s)))
    acc
  ) (ExprConstructor (Tag "Nil") (Arity 0))

extractStringList :: Value -> State EvalState [Text]
extractStringList v = case unApply v [] of
  Right (Tag "Nil", _, []) -> pure []
  Right (Tag "Cons", _, [h, t]) -> do
    headVal <- force h
    headStr <- case headVal of
      ValLiteral (LiteralString s) -> pure s
      _ -> throw $ RuntimeException "Expected String in list"
    tailVal <- force t
    tailStrs <- extractStringList tailVal
    pure (headStr : tailStrs)
  _ -> throw $ RuntimeException "Expected a list of strings"

wordsPrim :: Expr -> State EvalState Value
wordsPrim a = do
  val <- whnf a
  case val of
    ValLiteral (LiteralString s) -> whnf $ buildStringListExpr (T.words s)
    _ -> throw $ RuntimeException "words expects a String"

linesPrim :: Expr -> State EvalState Value
linesPrim a = do
  val <- whnf a
  case val of
    ValLiteral (LiteralString s) -> whnf $ buildStringListExpr (T.lines s)
    _ -> throw $ RuntimeException "lines expects a String"

unwordsPrim :: Expr -> State EvalState Value
unwordsPrim a = do
  val <- whnf a
  strs <- extractStringList val
  pure . ValLiteral . LiteralString $ T.unwords strs

unlinesPrim :: Expr -> State EvalState Value
unlinesPrim a = do
  val <- whnf a
  strs <- extractStringList val
  pure . ValLiteral . LiteralString $ T.unlines strs

floorPrim :: Expr -> State EvalState Value
floorPrim a = do
  val <- whnf a
  case val of
    ValLiteral (LiteralDouble d) -> pure . ValLiteral . LiteralInt $ floor d
    _ -> throw $ RuntimeException "floor expects a Double"

ceilingPrim :: Expr -> State EvalState Value
ceilingPrim a = do
  val <- whnf a
  case val of
    ValLiteral (LiteralDouble d) -> pure . ValLiteral . LiteralInt $ ceiling d
    _ -> throw $ RuntimeException "ceiling expects a Double"

roundPrim :: Expr -> State EvalState Value
roundPrim a = do
  val <- whnf a
  case val of
    ValLiteral (LiteralDouble d) -> pure . ValLiteral . LiteralInt $ round d
    _ -> throw $ RuntimeException "round expects a Double"

unpackValue :: Value -> State EvalState Text
unpackValue (ValLiteral (LiteralInt n)) = pure $ tshow n
unpackValue (ValLiteral (LiteralString s)) = pure s
unpackValue (ValLiteral (LiteralDouble f)) = pure $ tshow f
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
