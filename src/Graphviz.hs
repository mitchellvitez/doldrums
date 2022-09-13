{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphviz
  ( toGraphviz
  )
where

import Language
import Control.Monad.State
import Data.Foldable
import Data.Text

toGraphviz :: Expr -> Text
toGraphviz e = fold
  [ "digraph {\n  rankdir=BT\n  ordering=in\n  0 [label=\"main\"]\n  1 -> 0"
  , exprToGraphviz $ fst $ runState (label e) 0
  , "\n}"
  ]

tshow :: Show a => a -> Text
tshow = pack . show

node :: Integer -> Text -> Text
node n label = "\n  " <> tshow n <> " [label=\"" <> label <> "\"]"

pointsTo :: Integer -> Integer -> Text
pointsTo a b = "\n  " <> tshow a <> " -> " <> tshow b

getNext :: State Integer Integer
getNext = modify (+1) >> get

-- label nodes with an in-order traversal
label :: Expr -> State Integer (AnnotatedExpr Integer)
label (ExprInt x) = do
  n <- getNext
  pure $ AnnExprInt n x
label e@(ExprBool x) = do
  n <- getNext
  pure $ AnnExprBool n x
label e@(ExprString x) = do
  n <- getNext
  pure $ AnnExprString n x
label e@(ExprConstructor tag arity) = do
  n <- getNext
  pure $ AnnExprConstructor n tag arity
label e@(ExprDouble x) = do
  n <- getNext
  pure $ AnnExprDouble n x
label e@(ExprVariable v) = do
  n <- getNext
  pure $ AnnExprVariable n v
label (ExprApplication left right) = do
  n <- getNext
  labeledLeft <- label left
  labeledRight <- label right
  pure $ AnnExprApplication n labeledLeft labeledRight
label (ExprLambda name expr) = do
  n <- getNext
  labeledExpr <- label expr
  pure $ AnnExprLambda n name labeledExpr
label (ExprLet name binding body) = do
  n <- getNext
  labeledBinding <- label binding
  labeledBody <- label body
  pure $ AnnExprLet n name labeledBinding labeledBody
label _ = error "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms"

exprToGraphviz :: AnnotatedExpr Integer -> Text
exprToGraphviz (AnnExprInt n x) = node n $ tshow x
exprToGraphviz (AnnExprBool n b) = node n $ tshow b
exprToGraphviz (AnnExprString n s) = node n $ tshow s
exprToGraphviz (AnnExprDouble n d) = node n $ tshow d
exprToGraphviz (AnnExprConstructor n t a) =
  node n $ tshow $ "Pack{" <> tshow t <> "," <> tshow a <> "}"
exprToGraphviz (AnnExprVariable n v) = node n v
exprToGraphviz (AnnExprApplication n f x) = fold
  [ node n "App"
  , exprToGraphviz f
  , annotation f `pointsTo` n
  , exprToGraphviz x
  , annotation x `pointsTo` n
  ]
exprToGraphviz (AnnExprLet n name binding body) = fold
  [ node n $ "Let (" <> name <> ")"
  , exprToGraphviz binding
  , annotation binding `pointsTo` n
  , exprToGraphviz body
  , annotation body `pointsTo` n
  ]
exprToGraphviz (AnnExprLambda n name expr) = fold
  [ node n $ "Lam (" <> name <> ")"
  , exprToGraphviz expr
  , annotation expr `pointsTo` n
  ]
