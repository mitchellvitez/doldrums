{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module FixAst
  ( fixAst
  , singleExprForm
  )
where

import Language
import Derive (deriveInstances)
import Desugar (desugarProgram)
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

fixAst :: Program a -> Program a
fixAst =
  filterReachable .
  fixArities .
  desugarProgram .
  checkUniqueness .
  combineFunctions .
  deriveProgram .
  generateRecordAccessors

generateRecordAccessors :: Program a -> Program a
generateRecordAccessors program@Program{..} =
  let accessors = map (fmap (const (error "accessor"))) $ concatMap genAccessorsForDataDecl dataDeclarations
  in program { functions = functions <> accessors }

genAccessorsForDataDecl :: DataDeclaration -> [Function ()]
genAccessorsForDataDecl dd =
  [ let body = AnnExprVariable () (xArg tag fieldIdx)
        numArgs = length $ constructorArgTypes args
        patArgs = replicate numArgs PatternWildcard
        patArgsWithField = replaceNth fieldIdx (PatternVar (xArg tag fieldIdx)) patArgs
        patternArg = PatternConstructor tag patArgsWithField
    in Function () fieldName [patternArg] body
  | (tag, args) <- declarations dd
  , isRecordConstructor args
  , (fieldIdx, fieldName) <- zip [0..] (recordFieldNames args)
  ]
  where
    xArg :: Tag -> Int -> Name
    xArg (Tag t) i = Name $ "x_" <> t <> "_" <> T.pack (show i)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 y (_:xs) = y : xs
replaceNth n y (x:xs) = x : replaceNth (n-1) y xs

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
groupByName = Map.elems . Map.fromListWith (flip (<>)) . map (\f -> (name f, [f]))

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
      patternGroupKey (PatternRecord t _) = ConstructorKey t
      patternGroupKey (PatternRecordWildcard t) = ConstructorKey t

      isCatchAllPattern PatternWildcard = True
      isCatchAllPattern (PatternVar _) = True
      isCatchAllPattern _ = False

      (catchalls, specifics) = partition (isCatchAllPattern . fst) grouped

  in AnnExprCase annot (AnnExprVariable annot var)
    [Alternative pat (buildClauses annot vars subAlts) | (pat, subAlts) <- specifics <> catchalls]
buildClauses _ _ _ = error "unhandled build clauses"

data PatternGroupKey = CatchAllKey | LiteralKey Text | ConstructorKey Tag
  deriving (Eq, Ord)

-- convert Program to a single Expr (`let topLevelFunction = ... in main`)
singleExprForm :: Program a -> AnnotatedExpr a
singleExprForm program =
  case partition (\f -> name f == Name "main") (functions program) of
    ([], _) -> error "Couldn't find `main` function"
    ([main], otherFunctions) -> foldl' toSingle (body main) otherFunctions
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
lookupTag (DataDeclaration ((x, args):xs) dataType typeParams deriv : rest) tag
  | tag == x = Arity $ length args
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

