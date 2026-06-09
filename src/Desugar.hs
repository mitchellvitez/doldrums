{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Desugar
  ( desugarProgram )
where

import Language
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | match on expressions, return `Just newExpr` if a desugaring rule applies
desugarRules :: [DataDeclaration] -> AnnotatedExpr a -> Maybe (AnnotatedExpr a)
desugarRules _dds expr = case expr of
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

-- | Parse alternating (StringLiteral, value) pairs from a list of args
parseFieldPairs :: [AnnotatedExpr a] -> [(Name, AnnotatedExpr a)]
parseFieldPairs (AnnExprLiteral _ (LiteralString name) : val : rest) = (Name name, val) : parseFieldPairs rest
parseFieldPairs _ = []

-- | Desugar synthetic record construction: __rec_Foo "bar" e1 "baz" e2  →  Foo e1 e2
desugarSyntheticRecordConstruction :: a -> Tag -> [AnnotatedExpr a] -> [DataDeclaration] -> AnnotatedExpr a
desugarSyntheticRecordConstruction ann tag args dds =
  case getRecordFieldOrder tag dds of
    Just fieldNames ->
      let pairs = parseFieldPairs args
          fieldMap = Map.fromList pairs
          positionalArgs = map (\n -> fromMaybe (error $ "Missing field: " <> T.unpack (unName n)) (Map.lookup n fieldMap)) fieldNames
      in foldl (AnnExprApplication ann)
               (AnnExprConstructor ann tag (lookupTag dds tag))
               positionalArgs
    Nothing -> error $ "Record construction for non-record constructor: " <> show tag

-- | Desugar synthetic record wildcard: __wc__ "Foo"  →  Foo bar baz
desugarSyntheticRecordWildcard :: a -> [AnnotatedExpr a] -> [DataDeclaration] -> AnnotatedExpr a
desugarSyntheticRecordWildcard ann args dds =
  case args of
    [AnnExprLiteral _ (LiteralString tagStr)] ->
      let tag = Tag tagStr
      in case getRecordFieldOrder tag dds of
        Just fieldNames ->
          desugarSyntheticRecordConstruction ann tag (concat [[AnnExprLiteral ann (LiteralString (unName n)), AnnExprVariable ann n] | n <- fieldNames]) dds
        Nothing -> error $ "Record wildcard for non-record constructor: " <> show tag
    _ -> error $ "Invalid record wildcard encoding"

-- | Desugar synthetic record update: __upd__ expr "bar" e1  →  case expr of Foo old1 oldN -> Foo new1 newN
desugarSyntheticRecordUpdate :: a -> [AnnotatedExpr a] -> [DataDeclaration] -> AnnotatedExpr a
desugarSyntheticRecordUpdate ann (expr : rest) dds =
  let pairs = parseFieldPairs rest
  in case find (\(_, args) -> isRecordConstructor args && all (\(n,_) -> n `elem` recordFieldNames args) pairs)
              (concatMap declarations dds) of
    Just (tag, args) ->
      let fieldNames = recordFieldNames args
          updateVar i = Name $ "old_" <> unTag tag <> "_" <> T.pack (show i)
          oldVars = map updateVar [0..length fieldNames - 1]
          oldPattern = PatternConstructor tag (map PatternVar oldVars)
          fieldMap = Map.fromList pairs
          newArgs = zipWith (\i fn -> case Map.lookup fn fieldMap of
                                        Just newVal -> newVal
                                        Nothing     -> AnnExprVariable ann (updateVar i)
                            ) [0..] fieldNames
          newConstructor = foldl (AnnExprApplication ann)
                                 (AnnExprConstructor ann tag (lookupTag dds tag))
                                 newArgs
      in AnnExprCase ann expr [Alternative oldPattern newConstructor]
    Nothing -> error $ "Record update for unknown fields"
desugarSyntheticRecordUpdate _ _ _ = error "Invalid record update encoding"

-- | Desugar synthetic record dot: __dot__ expr "bar"  →  bar expr
desugarSyntheticRecordDot :: a -> [AnnotatedExpr a] -> [DataDeclaration] -> AnnotatedExpr a
desugarSyntheticRecordDot ann args _dds =
  case args of
    [expr, AnnExprLiteral _ (LiteralString fieldName)] ->
      AnnExprApplication ann (AnnExprVariable ann (Name fieldName)) expr
    _ -> error "Invalid record dot encoding"

-- | Get field names in declaration order for a record constructor
getRecordFieldOrder :: Tag -> [DataDeclaration] -> Maybe [Name]
getRecordFieldOrder tag dds =
  case concatMap declarations dds of
    decls -> case lookup tag decls of
      Just args | isRecordConstructor args -> Just (recordFieldNames args)
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
desugarProgram Program{..} =
  Program (map (desugarFunction dataDeclarations) functions)
          dataDeclarations
          typeSignatures
          typeclassDeclarations
          (map (desugarInstance dataDeclarations) instanceDeclarations)

desugarFunction :: [DataDeclaration] -> Function a -> Function a
desugarFunction dds f = f { args = map (desugarPattern dds) (args f), body = desugarExpr dds $ body f }

desugarInstance :: [DataDeclaration] -> InstanceDeclaration a -> InstanceDeclaration a
desugarInstance dds (InstanceDeclaration ctx cls ty meths) =
  InstanceDeclaration ctx cls ty [(n, desugarExpr dds m) | (n, m) <- meths]

desugarExpr :: [DataDeclaration] -> AnnotatedExpr a -> AnnotatedExpr a
desugarExpr dds expr =
  -- Check for synthetic record forms BEFORE recursive desugaring (mapExpr),
  -- so that partial application chains are never processed as records.
  case collectArgs expr of
    (AnnExprConstructor _ (Tag tag) _, args)
      | not (null args) && "__rec_" `T.isPrefixOf` tag ->
        desugarExpr dds $ desugarSyntheticRecordConstruction (annotation expr) (Tag $ T.drop 6 tag) args dds
      | not (null args) && tag == "__wc__" ->
        desugarExpr dds $ desugarSyntheticRecordWildcard (annotation expr) args dds
      | not (null args) && tag == "__upd__" ->
        desugarExpr dds $ desugarSyntheticRecordUpdate (annotation expr) args dds
      | not (null args) && tag == "__dot__" ->
        desugarExpr dds $ desugarSyntheticRecordDot (annotation expr) args dds
    _ ->
      let mappedExpr = mapExpr dds (desugarExpr dds) expr
      in case desugarRules dds mappedExpr of
        Just desugaredExpr -> desugaredExpr
        Nothing -> mappedExpr

mapExpr :: [DataDeclaration] -> (AnnotatedExpr a -> AnnotatedExpr a) -> AnnotatedExpr a -> AnnotatedExpr a
mapExpr _ _ (AnnExprVariable a n) = AnnExprVariable a n
mapExpr _ _ (AnnExprLiteral a l) = AnnExprLiteral a l
mapExpr _ _ (AnnExprConstructor a tag arity) = AnnExprConstructor a tag arity
mapExpr _ f (AnnExprApplication a e1 e2) = AnnExprApplication a (f e1) (f e2)
mapExpr _ f (AnnExprLet a bindings body) = AnnExprLet a [(name, f binding) | (name, binding) <- bindings] (f body)
mapExpr _ f (AnnExprLambda a n e) = AnnExprLambda a n (f e)
mapExpr d f (AnnExprCase a e alts) = AnnExprCase a (f e) (map (\(Alternative pat body) -> Alternative (desugarPattern d pat) (f body)) alts)


-- | Desugar record patterns to constructor patterns
desugarPattern :: [DataDeclaration] -> Pattern -> Pattern
desugarPattern dds = \case
  -- Foo { bar = p1, baz = p2 }  →  PatternConstructor Foo [p1, p2]  (in field order)
  PatternRecord tag fields ->
    case getRecordFieldOrder tag dds of
      Just fieldNames ->
        let fieldMap = Map.fromList fields
            positionalPats = map (\n -> fromMaybe PatternWildcard (Map.lookup n fieldMap)) fieldNames
        in PatternConstructor tag positionalPats
      Nothing -> error $ "Record pattern for non-record constructor: " <> show tag
  -- Foo { .. }  →  PatternConstructor Foo [PatternVar "bar", PatternVar "baz"]
  PatternRecordWildcard tag ->
    case getRecordFieldOrder tag dds of
      Just fieldNames -> PatternConstructor tag (map PatternVar fieldNames)
      Nothing -> error $ "Record wildcard pattern for non-record constructor: " <> show tag
  -- Pass through other patterns, but also desugar sub-patterns of constructor patterns
  PatternConstructor tag pats -> PatternConstructor tag (map (desugarPattern dds) pats)
  other -> other

lookupTag :: [DataDeclaration] -> Tag -> Arity
lookupTag [] tag = error $ "Could not find constructor: " <> show tag
lookupTag (DataDeclaration [] _dataType _typeParams _deriv : rest) tag = lookupTag rest tag
lookupTag (DataDeclaration ((x, args):xs) dataType typeParams deriv : rest) tag
  | tag == x = Arity $ length args
  | otherwise = lookupTag (DataDeclaration xs dataType typeParams deriv : rest) tag
