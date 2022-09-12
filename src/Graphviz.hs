module Graphviz where

import Language
import Control.Monad.State
import Data.Foldable
import Data.Text

data LabeledExpr a
  = LabeledExprVariable a Name
  | LabeledExprInt a Integer
  | LabeledExprBool a Bool
  | LabeledExprString a Text
  | LabeledExprDouble a Double
  | LabeledExprConstructor a Tag Arity
  | LabeledExprApplication a (LabeledExpr a) (LabeledExpr a)
  | LabeledExprLet a Name (LabeledExpr a) (LabeledExpr a)
  | LabeledExprLambda a Name (LabeledExpr a)
  deriving (Show, Eq, Ord)

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
label :: Expr -> State Integer (LabeledExpr Integer)
label e@(ExprInt x) = do
  n <- getNext
  pure $ LabeledExprInt n x
label e@(ExprBool x) = do
  n <- getNext
  pure $ LabeledExprBool n x
label e@(ExprString x) = do
  n <- getNext
  pure $ LabeledExprString n x
label e@(ExprConstructor tag arity) = do
  n <- getNext
  pure $ LabeledExprConstructor n tag arity
label e@(ExprDouble x) = do
  n <- getNext
  pure $ LabeledExprDouble n x
label e@(ExprVariable v) = do
  n <- getNext
  pure $ LabeledExprVariable n v
label (ExprApplication left right) = do
  n <- getNext
  labeledLeft <- label left
  labeledRight <- label right
  pure $ LabeledExprApplication n labeledLeft labeledRight
label (ExprLambda name expr) = do
  n <- getNext
  labeledExpr <- label expr
  pure $ LabeledExprLambda n name labeledExpr
label (ExprLet name binding body) = do
  n <- getNext
  labeledBinding <- label binding
  labeledBody <- label body
  pure $ LabeledExprLet n name labeledBinding labeledBody

labelOf :: LabeledExpr Integer -> Integer
labelOf (LabeledExprInt n _) = n
labelOf (LabeledExprVariable n _) = n
labelOf (LabeledExprApplication n _ _) = n
labelOf (LabeledExprBool n _) = n
labelOf (LabeledExprString n _) = n
labelOf (LabeledExprDouble n _) = n
labelOf (LabeledExprConstructor n _ _) = n
labelOf (LabeledExprLet n _ _ _) = n
labelOf (LabeledExprLambda n _ _) = n

exprToGraphviz :: LabeledExpr Integer -> Text
exprToGraphviz (LabeledExprInt n x) = node n $ tshow x
exprToGraphviz (LabeledExprBool n b) = node n $ tshow b
exprToGraphviz (LabeledExprString n s) = node n $ tshow s
exprToGraphviz (LabeledExprDouble n d) = node n $ tshow d
exprToGraphviz (LabeledExprConstructor n t a) =
  node n $ tshow $ "Pack{" <> tshow t <> "," <> tshow a <> "}"
exprToGraphviz (LabeledExprVariable n v) = node n v
exprToGraphviz (LabeledExprApplication n f x) = fold
  [ node n "App"
  , exprToGraphviz f
  , labelOf f `pointsTo` n
  , exprToGraphviz x
  , labelOf x `pointsTo` n
  ]
exprToGraphviz (LabeledExprLet n name binding body) = fold
  [ node n $ "Let (" <> name <> ")"
  , exprToGraphviz binding
  , labelOf binding `pointsTo` n
  , exprToGraphviz body
  , labelOf body `pointsTo` n
  ]
exprToGraphviz (LabeledExprLambda n name expr) = fold
  [ node n $ "Lam (" <> name <> ")"
  , exprToGraphviz expr
  , labelOf expr `pointsTo` n
  ]
