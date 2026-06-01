{-# LANGUAGE PatternSynonyms #-}

module FixAst
  ( fixAst
  , singleExprForm
  )
where

import Language
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

fixAst :: Program a -> Program a
fixAst =
  desugarProgram .
  fixArities .
  checkUniqueness

-- convert Program to a single Expr (`let topLevelExpr = ... in main`)
singleExprForm :: Program a -> AnnotatedExpr a
singleExprForm program@(Program funcs _) =
  foldl' toSingle (getMainExpr program) (filter (\(Function _ name _ _) -> name /= Name "main") funcs)

  where
    toSingle :: AnnotatedExpr a -> Function a -> AnnotatedExpr a
    toSingle expr func = AnnExprLet (annotation expr) (name func) (toNestedLambdas func) expr

    toNestedLambdas :: Function a -> AnnotatedExpr a
    toNestedLambdas (Function annot name args body) =
      foldr (AnnExprLambda annot) body args

    getMainExpr :: Program a -> AnnotatedExpr a
    getMainExpr (Program [] datas) = error "Couldn't find main"
    getMainExpr (Program (Function _ name _ body : restFuncs) datas) = case name of
      Name "main" -> body
      _ -> getMainExpr $ Program restFuncs datas


-- FIX ARITIES --

fixArities :: Program a -> Program a
fixArities (Program funcs datas) = Program (map (fixFunctionArities datas) funcs) datas

fixFunctionArities :: [DataDeclaration] -> Function a -> Function a
fixFunctionArities datas f@(Function _ _ _ body) =
  f { body = fixExprArities datas body }

fixExprArities :: [DataDeclaration] -> AnnotatedExpr a -> AnnotatedExpr a
fixExprArities _ e@(AnnExprVariable _ _) = e
fixExprArities _ e@(AnnExprInt _ _) = e
fixExprArities _ e@(AnnExprString _ _) = e
fixExprArities _ e@(AnnExprDouble _ _) = e
fixExprArities datas (AnnExprApplication a f x) = AnnExprApplication a (fixExprArities datas f) (fixExprArities datas x)
fixExprArities datas (AnnExprLet a name binding body) =
  AnnExprLet a name (fixExprArities datas binding) (fixExprArities datas body)
fixExprArities datas (AnnExprConstructor a tag _) = AnnExprConstructor a tag $ lookupTag datas tag
fixExprArities datas (AnnExprLambda a name expr) = AnnExprLambda a name (fixExprArities datas expr)
fixExprArities datas (AnnExprCase a expr alters) = AnnExprCase a (fixExprArities datas expr) alters

lookupTag :: [DataDeclaration] -> Tag -> Arity
lookupTag [] tag = error $ "Could not find constructor: " <> show tag
lookupTag (DataDeclaration [] _dataType : rest) tag = lookupTag rest tag
lookupTag (DataDeclaration ((tagX, arity):xs) dataType : rest) tag
  | tag == tagX = arity
  | otherwise = lookupTag (DataDeclaration xs dataType : rest) tag


-- UNIQUENESS --

checkUniqueness :: Program a -> Program a
checkUniqueness = checkUniqueConstructors . checkUniqueFunctions

checkUniqueFunctions :: Program a -> Program a
checkUniqueFunctions program@(Program funcs datas) =
  case findDuplicate Set.empty $ map name funcs of
    Nothing -> program
    Just (Name name) -> error $ "Duplicate function: " <> Text.unpack name

checkUniqueConstructors :: Program a -> Program a
checkUniqueConstructors program@(Program funcs datas) =
  case findDuplicate Set.empty . map fst $ concatMap declarations datas of
    Nothing -> program
    Just (Tag tag) -> error $ "Duplicate constructor: " <> Text.unpack tag

findDuplicate :: Ord a => Set a -> [a] -> Maybe a
findDuplicate seen [] = Nothing
findDuplicate seen (x:xs) =
  if x `Set.member` seen
  then Just x
  else findDuplicate (Set.insert x seen) xs


-- DESUGARING --

-- | match on expressions, return `Just newExpr` if a desugaring rule applies
desugarRules :: AnnotatedExpr a -> Maybe (AnnotatedExpr a)
desugarRules = \case
  -- a && b  ~>  case a of { True -> b; False -> False }
  BinOp ann a b "&&" ->
    Just $ AnnExprCase ann a
      [ Alternative (Tag "True") [] b
      , Alternative (Tag "False") [] $ exprFalse ann
      ]
  -- a || b  ~>  case a of { False -> b; True -> True }
  BinOp ann a b "||" ->
    Just $ AnnExprCase ann a
      [ Alternative (Tag "False") [] b
      , Alternative (Tag "True") [] $ exprTrue ann
      ]

  -- a == b  ~>  case compare a b of { LT -> False; EQ -> True; GT -> False }
  BinOp ann a b "==" ->
    desugarComparisonOperator ann a b (False, True, False)
  -- a != b  ~>  case compare a b of { LT -> True; EQ -> False; GT -> True }
  BinOp ann a b "!=" ->
    desugarComparisonOperator ann a b (True, False, True)
  -- etc.
  BinOp ann a b "<" ->
    desugarComparisonOperator ann a b (True, False, False)
  BinOp ann a b ">" ->
    desugarComparisonOperator ann a b (False, False, True)
  BinOp ann a b "<=" ->
    desugarComparisonOperator ann a b (True, True, False)
  BinOp ann a b ">=" ->
    desugarComparisonOperator ann a b (False, True, True)
  _ -> Nothing

pattern BinOp :: a -> AnnotatedExpr a -> AnnotatedExpr a -> Text.Text -> AnnotatedExpr a
pattern BinOp ann a b op <-
  AnnExprApplication ann (AnnExprApplication _ (AnnExprVariable _ (Name op)) a) b

desugarComparisonOperator :: a -> AnnotatedExpr a -> AnnotatedExpr a -> (Bool, Bool, Bool) -> Maybe (AnnotatedExpr a)
desugarComparisonOperator ann a b (lt, eq, gt) =
  Just $ AnnExprCase ann (AnnExprApplication ann (AnnExprApplication ann (AnnExprVariable ann "compare") a) b)
    [ Alternative (Tag "LT") [] $ (toExprBool lt) ann
    , Alternative (Tag "EQ") [] $ (toExprBool eq) ann
    , Alternative (Tag "GT") [] $ (toExprBool gt) ann
    ]

toExprBool False = exprFalse
toExprBool True = exprTrue
exprFalse ann = AnnExprConstructor ann (Tag "False") (Arity 0)
exprTrue ann = AnnExprConstructor ann (Tag "True") (Arity 0)

desugarProgram :: Program a -> Program a
desugarProgram (Program funcs decls) =
  Program (map desugarFunction funcs) decls

desugarFunction :: Function a -> Function a
desugarFunction f = f { body = desugarExpr $ body f }

desugarExpr :: AnnotatedExpr a -> AnnotatedExpr a
desugarExpr expr =
  let mappedExpr = mapExpr desugarExpr expr
  in case desugarRules mappedExpr of
    Just desugaredExpr -> desugaredExpr
    Nothing -> mappedExpr

mapExpr :: (AnnotatedExpr a -> AnnotatedExpr a) -> AnnotatedExpr a -> AnnotatedExpr a
mapExpr f (AnnExprVariable a n) = AnnExprVariable a n
mapExpr f (AnnExprInt a n) = AnnExprInt a n
mapExpr f (AnnExprString a s) = AnnExprString a s
mapExpr f (AnnExprDouble a d) = AnnExprDouble a d
mapExpr f (AnnExprConstructor a tag arity) = AnnExprConstructor a tag arity
mapExpr f (AnnExprApplication a e1 e2) = AnnExprApplication a (f e1) (f e2)
mapExpr f (AnnExprLet a n b body) = AnnExprLet a n (f b) (f body)
mapExpr f (AnnExprLambda a n e) = AnnExprLambda a n (f e)
mapExpr f (AnnExprCase a e alts) = AnnExprCase a (f e) (map (\(Alternative tag names body) -> Alternative tag names (f body)) alts)
