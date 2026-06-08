{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module FixAst
  ( fixAst
  , singleExprForm
  )
where

import Language
import Derive (deriveInstances)
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

fixAst :: Program a -> Program a
fixAst = filterReachable . desugarProgram . fixArities . checkUniqueness . combineFunctions . deriveProgram

-- generate instance declaration code from `deriving` clauses
deriveProgram :: Program a -> Program a
deriveProgram program@Program{..} =
  let derived = map adjustAnnotation $ concatMap deriveInstances dataDeclarations
      adjustAnnotation instanceDeclaration@InstanceDeclaration{..} =
        instanceDeclaration { instanceMethods = [(name, fmap (const (error "derive")) method) | (name, method) <- instanceMethods] }
  in program { instanceDeclarations = instanceDeclarations <> derived }

-- group functions by name and combine pattern-matching clauses into one
combineFunctions :: Program a -> Program a
combineFunctions program@Program{..} =
  program { functions = concatMap combineGroup $ groupByName functions }

-- allow for multiple definitions (i.e. multiple pattern matches with same function name)
groupByName :: [Function a] -> [[Function a]]
groupByName = Map.elems . Map.fromListWith (flip (++)) . map (\f -> (name f, [f]))

combineGroup :: [Function a] -> [Function a]
combineGroup [f] = [f]
combineGroup fs@(Function annot name pats _ : _) =
  let arity = length pats
      varNames = [Name $ "x" <> T.pack (show i) | i <- [1..arity]]
      varPats = map PatternVar varNames
      clauses = [(args, body) | Function _ _ args body <- fs]
  in [Function annot name varPats (buildClauses annot varNames clauses)]
combineGroup _ = error "invalid input to combineGroup"

buildClauses :: a -> [Name] -> [([Pattern], AnnotatedExpr a)] -> AnnotatedExpr a
buildClauses _ [var] alts =
  AnnExprCase ann (AnnExprVariable ann var)
    [Alternative pat body | ([pat], body) <- alts]
  where
    ann = annotation . snd $ fromMaybe (error "empty case expression") (listToMaybe alts)
buildClauses annot (var:vars) alts =
  let grouped = Map.elems $ Map.fromListWith
        (\(p1, s1) (p2, s2) -> (chooseRepresentative p1 p2, s1 <> s2))
        [ (patternGroupKey p, (p, [(pats, body)])) | (p:pats, body) <- alts]

      chooseRepresentative p1 p2 = case (p1, p2) of
        (PatternVar _, _) -> p1
        (_, PatternVar _) -> p2
        _ -> p1

      patternGroupKey PatternWildcard = CatchAllKey
      patternGroupKey (PatternVar _) = CatchAllKey
      patternGroupKey (PatternLiteral l) = LiteralKey (T.pack $ show l)
      patternGroupKey (PatternConstructor t _) = ConstructorKey t

  in AnnExprCase annot (AnnExprVariable annot var)
    [Alternative pat (buildClauses annot vars subAlts) | (pat, subAlts) <- grouped]
buildClauses _ _ _ = error "unhandled build clauses"

data PatternGroupKey = CatchAllKey | LiteralKey Text | ConstructorKey Tag
  deriving (Eq, Ord)

-- convert Program to a single Expr (`let topLevelFunction = ... in main`)
singleExprForm :: Program a -> AnnotatedExpr a
singleExprForm program =
  case partition (\f -> f.name == Name "main") program.functions of
    ([], _) -> error "Couldn't find `main` function"
    ([main], otherFunctions) -> foldl' toSingle main.body otherFunctions
    _ -> error "two or more `main` functions exist"

toSingle :: AnnotatedExpr a -> Function a -> AnnotatedExpr a
toSingle expr func = AnnExprLet (annotation expr) [(name func, toNestedLambdas func)] expr

toNestedLambdas :: Function a -> AnnotatedExpr a
toNestedLambdas (Function annot _ args body) =
  foldr (patternToLambda annot) body args

patternToLambda :: a -> Pattern -> AnnotatedExpr a -> AnnotatedExpr a
patternToLambda annot (PatternVar n) body = AnnExprLambda annot n body
patternToLambda annot pat body =
  AnnExprLambda annot (Name "pat") (AnnExprCase annot (AnnExprVariable annot (Name "pat")) [Alternative pat body])

-- FIX ARITIES --

fixArities :: Program a -> Program a
fixArities program@Program{..} = program
  { functions = map (fixFunctionArities dataDeclarations) functions
  , instanceDeclarations = map (fixInstanceArities dataDeclarations) instanceDeclarations
  }

fixInstanceArities :: [DataDeclaration] -> InstanceDeclaration a -> InstanceDeclaration a
fixInstanceArities datas (InstanceDeclaration ctx cls ty meths) =
  InstanceDeclaration ctx cls ty [(n, fixExprArities datas m) | (n, m) <- meths]

fixFunctionArities :: [DataDeclaration] -> Function a -> Function a
fixFunctionArities datas f@(Function _ _ _ body) =
  f { body = fixExprArities datas body }

fixExprArities :: [DataDeclaration] -> AnnotatedExpr a -> AnnotatedExpr a
fixExprArities _ e@(AnnExprVariable _ _) = e
fixExprArities _ e@(AnnExprLiteral _ _) = e
fixExprArities datas (AnnExprApplication a f x) = AnnExprApplication a (fixExprArities datas f) (fixExprArities datas x)
fixExprArities datas (AnnExprLet a bindings body) =
  AnnExprLet a [(name, fixExprArities datas binding) | (name, binding) <- bindings] (fixExprArities datas body)
fixExprArities datas (AnnExprConstructor a tag _) = AnnExprConstructor a tag $ lookupTag datas tag
fixExprArities datas (AnnExprLambda a name expr) = AnnExprLambda a name (fixExprArities datas expr)
fixExprArities datas (AnnExprCase a expr alters) = AnnExprCase a (fixExprArities datas expr) $ map fixAlt alters
  where fixAlt (Alternative pat body) = Alternative pat $ fixExprArities datas body

lookupTag :: [DataDeclaration] -> Tag -> Arity
lookupTag [] tag = error $ "Could not find constructor: " <> show tag
lookupTag (DataDeclaration [] _dataType _typeParams _deriv : rest) tag = lookupTag rest tag
lookupTag (DataDeclaration ((x, typeRefs):xs) dataType typeParams deriv : rest) tag
  | tag == x = Arity $ length typeRefs
  | otherwise = lookupTag (DataDeclaration xs dataType typeParams deriv : rest) tag


-- UNIQUENESS --

checkUniqueness :: Program a -> Program a
checkUniqueness = checkUniqueConstructors . checkUniqueFunctions

checkUniqueFunctions :: Program a -> Program a
checkUniqueFunctions program@(Program funcs _ _ _ _) =
  case findDuplicate Set.empty $ map name funcs of
    Nothing -> program
    Just (Name name) -> error $ "Duplicate function: " <> T.unpack name

checkUniqueConstructors :: Program a -> Program a
checkUniqueConstructors program@(Program _ datas _ _ _) =
  case findDuplicate Set.empty . map fst $ concatMap declarations datas of
    Nothing -> program
    Just (Tag tag) -> error $ "Duplicate constructor: " <> T.unpack tag

findDuplicate :: Ord a => Set a -> [a] -> Maybe a
findDuplicate _ [] = Nothing
findDuplicate seen (x:xs) =
  if x `Set.member` seen
  then Just x
  else findDuplicate (Set.insert x seen) xs

-- REACHABILITY --

filterReachable :: Program a -> Program a
filterReachable program@Program{..}
  | Name "main" `Set.notMember` functionNames = program
  | otherwise = program
      { functions = filter ((`Set.member` live) . name) functions
      , typeSignatures = filter (\(name, _) -> name `Set.member` live) typeSignatures
      }
  where
    functionNames = Set.fromList $ map name functions
    live = until (\s -> step s == s) step $
      Set.singleton (Name "main") `Set.union`
      Set.unions (concatMap (map (refs . snd) . instanceMethods) instanceDeclarations)
    step s = s `Set.union` Set.unions (map (refs . body) $ filter ((`Set.member` s) . name) functions)
    refs (AnnExprVariable _ name)
      | name `Set.member` functionNames = Set.singleton name
      | otherwise = Set.empty
    refs (AnnExprApplication _ f x) = refs f `Set.union` refs x
    refs (AnnExprLet _ bindings body) = Set.unions (map (refs . snd) bindings) `Set.union` refs body
    refs (AnnExprLambda _ _ expr) = refs expr
    refs (AnnExprCase _ scrutinee alts) = refs scrutinee `Set.union` Set.unions [refs body | Alternative _ body <- alts]
    refs _ = Set.empty

-- DESUGARING --

-- | match on expressions, return `Just newExpr` if a desugaring rule applies
desugarRules :: AnnotatedExpr a -> Maybe (AnnotatedExpr a)
desugarRules = \case
  -- x : xs  ~>  Cons x xs
  AnnExprVariable ann (Name ":") ->
    Just $ AnnExprConstructor ann (Tag "Cons") (Arity 2)
  -- a && b  ~>  case a of { True -> b; False -> False }
  BinOp ann a b "&&" ->
    Just $ AnnExprCase ann a
      [ Alternative (PatternConstructor (Tag "True") []) b
      , Alternative (PatternConstructor (Tag "False") []) $ exprFalse ann
      ]
  -- a || b  ~>  case a of { False -> b; True -> True }
  BinOp ann a b "||" ->
    Just $ AnnExprCase ann a
      [ Alternative (PatternConstructor (Tag "False") []) b
      , Alternative (PatternConstructor (Tag "True") []) $ exprTrue ann
      ]

  -- etc.
  BinOp ann a b "<" ->
    desugarComparisonOperator ann a b (True, False, False)
  BinOp ann a b ">" ->
    desugarComparisonOperator ann a b (False, False, True)
  BinOp ann a b "<=" ->
    desugarComparisonOperator ann a b (True, True, False)
  BinOp ann a b ">=" ->
    desugarComparisonOperator ann a b (False, True, True)

  -- if cond then t else e  ~>  case cond of { True -> t; False -> e }
  AnnExprApplication ann
    (AnnExprApplication _
      (AnnExprApplication _ (AnnExprVariable _ (Name "if")) cond)
      trueVal)
    falseVal ->
      Just $ AnnExprCase ann cond
        [ Alternative (PatternConstructor (Tag "True") []) trueVal
        , Alternative (PatternConstructor (Tag "False") []) falseVal
        ]
  _ -> Nothing

pattern BinOp :: a -> AnnotatedExpr a -> AnnotatedExpr a -> Text -> AnnotatedExpr a
pattern BinOp ann a b op <-
  AnnExprApplication ann (AnnExprApplication _ (AnnExprVariable _ (Name op)) a) b

desugarComparisonOperator :: a -> AnnotatedExpr a -> AnnotatedExpr a -> (Bool, Bool, Bool) -> Maybe (AnnotatedExpr a)
desugarComparisonOperator ann a b (lt, eq, gt) =
  Just $ AnnExprCase ann (AnnExprApplication ann (AnnExprApplication ann (AnnExprVariable ann "compare") a) b)
    [ Alternative (PatternConstructor (Tag "LT") []) $ (toExprBool lt) ann
    , Alternative (PatternConstructor (Tag "EQ") []) $ (toExprBool eq) ann
    , Alternative (PatternConstructor (Tag "GT") []) $ (toExprBool gt) ann
    ]

toExprBool :: Bool -> a -> AnnotatedExpr a
toExprBool False = exprFalse
toExprBool True = exprTrue

exprFalse :: a -> AnnotatedExpr a
exprFalse ann = AnnExprConstructor ann (Tag "False") (Arity 0)

exprTrue :: a -> AnnotatedExpr a
exprTrue ann = AnnExprConstructor ann (Tag "True") (Arity 0)

desugarProgram :: Program a -> Program a
desugarProgram (Program funcs decls sigs cls insts) =
  Program (map desugarFunction funcs) decls sigs cls (map desugarInstance insts)

desugarFunction :: Function a -> Function a
desugarFunction f = f { body = desugarExpr $ body f }

desugarInstance :: InstanceDeclaration a -> InstanceDeclaration a
desugarInstance (InstanceDeclaration ctx cls ty meths) =
  InstanceDeclaration ctx cls ty [(n, desugarExpr m) | (n, m) <- meths]

desugarExpr :: AnnotatedExpr a -> AnnotatedExpr a
desugarExpr expr =
  let mappedExpr = mapExpr desugarExpr expr
  in case desugarRules mappedExpr of
    Just desugaredExpr -> desugaredExpr
    Nothing -> mappedExpr

mapExpr :: (AnnotatedExpr a -> AnnotatedExpr a) -> AnnotatedExpr a -> AnnotatedExpr a
mapExpr _ (AnnExprVariable a n) = AnnExprVariable a n
mapExpr _ (AnnExprLiteral a l) = AnnExprLiteral a l
mapExpr _ (AnnExprConstructor a tag arity) = AnnExprConstructor a tag arity
mapExpr f (AnnExprApplication a e1 e2) = AnnExprApplication a (f e1) (f e2)
mapExpr f (AnnExprLet a bindings body) = AnnExprLet a [(name, f binding) | (name, binding) <- bindings] (f body)
mapExpr f (AnnExprLambda a n e) = AnnExprLambda a n (f e)
mapExpr f (AnnExprCase a e alts) = AnnExprCase a (f e) (map (\(Alternative pat body) -> Alternative pat (f body)) alts)
