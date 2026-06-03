module STG where

import Language
import FixAst (singleExprForm)
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set

type StgName = Text

data StgLiteral
  = StgLitInt Integer
  | StgLitString Text
  | StgLitDouble Double
  deriving (Show)

data StgAtom
  = StgAtomVar StgName
  | StgAtomLit StgLiteral
  deriving (Show)

data StgExpr
  = StgAtom StgAtom
  | StgApp StgName [StgAtom]
  | StgLet [StgBinding] StgExpr
  | StgCase StgAtom [StgAlt]
  deriving (Show)

data StgBinding = StgBinding
  { stgName :: StgName
  , stgFreeVars :: [StgName]
  , stgArgs :: [StgName]
  , stgBody :: StgExpr
  } deriving (Show)

data StgAlt = StgAlt [StgBinding] StgExpr
  deriving (Show)

compileStg :: Program a -> StgExpr
compileStg program =
  let prog = fmap (const ()) program
      expr = singleExprForm prog
      stg = evalState (toStg expr) 0
      stgFvs = annotateFreeVars Set.empty stg
  in stgFvs

toStg :: Expr -> State Int StgExpr
toStg e = case e of
  ExprLiteral (LiteralInt n) -> pure . StgAtom . StgAtomLit $ StgLitInt n
  ExprLiteral (LiteralString s) -> pure . StgAtom . StgAtomLit $ StgLitString s
  ExprLiteral (LiteralFloat d) -> pure . StgAtom . StgAtomLit $ StgLitDouble d
  ExprVariable v -> pure . StgAtom . StgAtomVar $ unName v
  ExprConstructor tag _ -> pure . StgAtom . StgAtomVar $ unTag tag
  ExprLet bindings body -> do
    stgBody <- toStg body
    stgBindings <- mapM (\(name, bind) -> case bind of
      lam@(ExprLambda {}) -> do
        let (args, body') = collectLambdas lam
        stgBody' <- toStg body'
        pure $ StgBinding (unName name) [] (map unName args) stgBody'
      _ -> do
        stgBind <- toStg bind
        pure $ StgBinding (unName name) [] [] stgBind) bindings
    pure $ StgLet stgBindings stgBody
  lam@(ExprLambda _ _) -> do
    let (args, body) = collectLambdas lam
    name <- freshName
    stgBody <- toStg body
    pure $ StgLet [StgBinding name [] (map unName args) stgBody]
                  (StgAtom (StgAtomVar name))
  ExprCase scrutinee alts -> do
    (scrutAtom, scrutBinds) <- atomize scrutinee
    stgAlts <- mapM toStgAlt alts
    pure . StgLet scrutBinds $ StgCase scrutAtom stgAlts
  ExprApplication f a -> do
    (fAtom, fBinds) <- atomize f
    (aAtom, aBinds) <- atomize a
    case fAtom of
      StgAtomVar name ->
        pure . StgLet (fBinds <> aBinds) $ StgApp name [aAtom]
      _ -> error "toStg: function position must be a variable after atomization"

toStgAlt :: CaseAlternative () -> State Int StgAlt
toStgAlt (Alternative pat body) = do
  stgBody <- toStg body
  let pvars = patternNames pat
      bindings = [StgBinding (unName v) [] [] (StgAtom . StgAtomVar $ unName v) | v <- pvars]
  pure $ StgAlt bindings stgBody

atomize :: Expr -> State Int (StgAtom, [StgBinding])
atomize = \case
  ExprVariable v -> pure (StgAtomVar (unName v), [])
  ExprLiteral (LiteralInt n) -> pure (StgAtomLit (StgLitInt n), [])
  ExprLiteral (LiteralString s) -> pure (StgAtomLit (StgLitString s), [])
  ExprLiteral (LiteralFloat d) -> pure (StgAtomLit (StgLitDouble d), [])
  ExprConstructor tag _ -> pure (StgAtomVar (unTag tag), [])
  e -> do
    name <- freshName
    stgBody <- toStg e
    pure (StgAtomVar name, [StgBinding name [] [] stgBody])

collectLambdas :: Expr -> ([Name], Expr)
collectLambdas = \case
  ExprLambda x expr ->
    let (xs, body) = collectLambdas expr
    in (x:xs, body)
  e -> ([], e)

annotateFreeVars :: Set StgName -> StgExpr -> StgExpr
annotateFreeVars bound = \case
  StgLet bindings body ->
    let newBound = Set.union bound . Set.fromList $ map stgName bindings
        annotatedBindings = map annotateBinding bindings
    in StgLet annotatedBindings $ annotateFreeVars newBound body
  StgCase atom alts ->
    StgCase atom $ map annotateAlt alts
  other -> other
  where
    annotateBinding binding =
      let localBound = Set.union bound . Set.fromList $ stgArgs binding
          freeVars = Set.toList $ Set.difference (freeVarsExpr localBound $ stgBody binding) bound
      in binding { stgFreeVars = freeVars }

    annotateAlt (StgAlt bindings body) =
      let altBound = Set.union bound . Set.fromList $ map stgName bindings
      in StgAlt bindings $ annotateFreeVars altBound body

freeVarsExpr :: Set StgName -> StgExpr -> Set StgName
freeVarsExpr bound = \case
  StgAtom atom -> freeVarsAtom atom `Set.difference` bound
  StgApp name args ->
    Set.insert name (foldMap freeVarsAtom args) `Set.difference` bound
  StgLet bindings body ->
    let letBound = Set.fromList (map stgName bindings)
        fvBindings = foldMap
          (\b -> freeVarsExpr (Set.union bound . Set.fromList $ stgArgs b)
                              (stgBody b))
          bindings
        fvBody = freeVarsExpr (Set.union bound letBound) body
    in (fvBindings <> fvBody) `Set.difference` letBound
  StgCase atom alts ->
    freeVarsAtom atom `Set.difference` bound
      <> foldMap f alts
    where
      f (StgAlt bindings body) =
        freeVarsExpr (Set.union bound . Set.fromList $ map stgName bindings) body

freeVarsAtom :: StgAtom -> Set StgName
freeVarsAtom = \case
  StgAtomVar name -> Set.singleton name
  StgAtomLit _ -> Set.empty

freshName :: State Int StgName
freshName = do
  n <- get
  put (n + 1)
  pure $ "stg_" <> Text.pack (show n)
