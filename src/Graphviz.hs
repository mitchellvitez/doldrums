{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Graphviz
  ( toGraphviz
  )
where

import Language
import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

-- runs two passes:
--   1. label each node in the expr with unique integer
--   2. use the labeled expr tree to generate graphviz
toGraphviz :: Program Void -> Text
toGraphviz program = fold
  [ "digraph {\n  rankdir=BT\n  ordering=in\n"
  , programToGraphviz . fst $ runState (labelProgram program) (-1, Map.fromList [(Name "main", 0)])
  , "\n}"
  ]

tshow :: Show a => a -> Text
tshow = T.pack . show

node :: Integer -> Text -> Text
node n label = "\n  " <> tshow n <> " [label=\"" <> label <> "\"]"

boxNode :: Integer -> Text -> Text
boxNode n label = "\n  " <> tshow n <> " [label=\"" <> label <> "\", shape=\"box\"]"

pointsTo :: Integer -> Integer -> Text
pointsTo a b = "\n  " <> tshow a <> " -> " <> tshow b

type LabelState = (Integer, Map Name Integer)

getNext :: State LabelState Integer
getNext = modify (\(n, env) -> (n+1, env)) >> fst <$> get

labelProgram :: Program Void -> State LabelState (Program Integer)
labelProgram (Program funcs datas) = do
  let functionNames = fmap name funcs
      zipped = zip functionNames $ repeat (-1)
  put (0, Map.fromList zipped)
  newFuncs <- mapM labelFunction funcs
  newFuncs' <- mapM labelFunctionAndExprs newFuncs
  pure $ Program newFuncs' datas

labelFunction :: Function Void -> State LabelState (Function Integer)
labelFunction (Function annot name args body) = do
  getNext
  (n, env) <- get
  when (name `Map.member` env) $ error "duplication function with same name"
  let env' = Map.insert name n env
  put (n, env')
  pure $ Function n name args (const (-1) <$> body)

labelFunctionAndExprs :: Function Integer -> State LabelState (Function Integer)
labelFunctionAndExprs (Function annot name args body) = do
  labeledBody <- labelExpr (const Language.void <$> body)
  pure $ Function annot name args labeledBody

labelExpr :: Expr -> State LabelState (AnnotatedExpr Integer)
labelExpr (ExprInt x) = do
  n <- getNext
  pure $ AnnExprInt n x
labelExpr e@(ExprString x) = do
  n <- getNext
  pure $ AnnExprString n x
labelExpr e@(ExprConstructor tag arity) = do
  n <- getNext
  pure $ AnnExprConstructor n tag arity
labelExpr e@(ExprDouble x) = do
  n <- getNext
  pure $ AnnExprDouble n x
labelExpr e@(ExprVariable var) = do
  (_, env) <- get
  case Map.lookup var env of
    Nothing -> do
      n <- getNext
      pure $ AnnExprVariable n var
    Just existing ->
      pure $ AnnExprVariable existing var
labelExpr (ExprApplication left right) = do
  n <- getNext
  labeledLeft <- labelExpr left
  labeledRight <- labelExpr right
  pure $ AnnExprApplication n labeledLeft labeledRight
labelExpr (ExprLambda name expr) = do
  n <- getNext
  labeledExpr <- labelExpr expr
  pure $ AnnExprLambda n name labeledExpr
labelExpr (ExprLet name binding body) = do
  n <- getNext
  labeledBinding <- labelExpr binding
  labeledBody <- labelExpr body
  pure $ AnnExprLet n name labeledBinding labeledBody
labelExpr (ExprCase scrutinee alts) = do
  n <- getNext
  labeledScrutinee <- labelExpr scrutinee
  labeledAlts <- forM alts $ \(Alternative n vars expr) -> do
    labeledExpr <- labelExpr expr
    pure $ Alternative n vars labeledExpr
  pure $ AnnExprCase n labeledScrutinee labeledAlts
labelExpr _ = error "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms"

programToGraphviz :: Program Integer -> Text
programToGraphviz (Program funcs datas) =
  fold $ functionToGraphviz (Set.fromList $ fmap name funcs) <$> funcs

functionToGraphviz :: Set Name -> Function Integer -> Text
functionToGraphviz functionNames (Function annot name args body) = fold
  [ boxNode annot $ unName name
  , exprToGraphviz functionNames body
  , annotation body `pointsTo` annot
  ]

exprToGraphviz :: Set Name -> AnnotatedExpr Integer -> Text
exprToGraphviz _ (AnnExprInt n x) = node n $ tshow x
exprToGraphviz _ (AnnExprString n s) = node n $ tshow s
exprToGraphviz _ (AnnExprDouble n d) = node n $ tshow d
exprToGraphviz _ (AnnExprConstructor n (Tag t) a) =
  node n t
exprToGraphviz functionNames (AnnExprVariable n v) =
  if v `elem` functionNames then "" else node n (unName v)
exprToGraphviz functionNames (AnnExprApplication n f x) = fold
  [ node n "App"
  , exprToGraphviz functionNames f
  , annotation f `pointsTo` n
  , exprToGraphviz functionNames x
  , annotation x `pointsTo` n
  ]
exprToGraphviz functionNames (AnnExprLet n name binding body) = fold $
  [ toGraphviz (unName name) binding
  , exprToGraphviz functionNames body
  , annotation body `pointsTo` n
  ]
  where
    toGraphviz name binding = fold
      [ node n $ "Let (" <> name <> ")"
      , exprToGraphviz functionNames binding
      , annotation binding `pointsTo` n
      ]
exprToGraphviz functionNames (AnnExprLambda n name expr) = fold
  [ node n $ "Lam (" <> unName name <> ")"
  , exprToGraphviz functionNames expr
  , annotation expr `pointsTo` n
  ]
exprToGraphviz functionNames (AnnExprCase n expr alts) = fold
  [ node n $ "Case"
  , exprToGraphviz functionNames expr
  , annotation expr `pointsTo` n
  ]
  <> fold (Prelude.map toNodes alts)
  where
    toNodes (Alternative _n _names alt) = fold
      [ exprToGraphviz functionNames alt
      , annotation alt `pointsTo` n
      ]
