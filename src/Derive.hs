{-# LANGUAGE RecordWildCards #-}

module Derive
  ( deriveInstances
  )
where

import Language
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

{- | Generate @instance@ code from @deriving@ clauses attached to data declarations

This generated code is written directly as AST nodes

The currently supported instance derivations are @Eq@, @Ord@, and @Show@

For example, @deriving Eq@ after a @MyType@ data declaration generates code like:

@
instance Eq MyType where
  (==) = \\x -> \\y -> case x of
    MyTypeConstructor1 -> case y of
      MyTypeConstructor1 -> True
      _ -> False
    ...
  (/=) = ...
@
-}
deriveInstances :: DataDeclaration -> [InstanceDeclaration ()]
deriveInstances dd = concatMap (\n -> deriveInstance dd n) (derivingClauses dd)

deriveInstance :: DataDeclaration -> Name -> [InstanceDeclaration ()]
deriveInstance dd (Name "Eq")   = [deriveEq dd]
deriveInstance dd (Name "Ord")  = [deriveOrd dd]
deriveInstance dd (Name "Show") = [deriveShow dd]
deriveInstance _  name          = error $ "Cannot derive " <> T.unpack (unName name)

buildContext :: Name -> DataDeclaration -> [(Name, TypeHint)]
buildContext className DataDeclaration{..} =
  let usedParams = concatMap (\(_, args) -> concatMap (typeRefVars . constructorArgType) args) declarations
      params = filter (`elem` usedParams) typeParameters
  in map (\n -> (className, TypeHintVar n)) params

typeRefVars :: TypeRef -> [Name]
typeRefVars (TypeRefVar n) = [n]
typeRefVars (TypeRefConstructor _) = []
typeRefVars (TypeRefApp _ args) = concatMap typeRefVars args

buildInstanceTypes :: DataDeclaration -> [TypeHint]
buildInstanceTypes DataDeclaration{..} =
  let args = map TypeHintVar typeParameters
  in case args of
    [] -> [TypeHintConstructor dataType]
    _  -> [TypeHintApp dataType args]

-- | Make a locally-unique name for a constructor argument on the x-side
xArg :: Tag -> Int -> Name
xArg (Tag t) i = Name $ "x" <> t <> T.pack (show i)

-- | y-side argument name
yArg :: Tag -> Int -> Name
yArg (Tag t) i = Name $ "y" <> t <> T.pack (show i)

-- === Eq ===

deriveEq :: DataDeclaration -> InstanceDeclaration ()
deriveEq dd =
  InstanceDeclaration
    { instanceContext = buildContext (Name "Eq") dd
    , instanceClass = Name "Eq"
    , instanceTypes = buildInstanceTypes dd
    , instanceMethods =
        [ (Name "==", genEq (declarations dd))
        , (Name "/=", genNeq (declarations dd))
        ]
    }

genEq :: [(Tag, [ConstructorArg])] -> AnnotatedExpr ()
genEq constructors =
  let x = Name "x"
      y = Name "y"
  in exprLam x $ exprLam y $ eqCaseOnX constructors x y

genNeq :: [(Tag, [ConstructorArg])] -> AnnotatedExpr ()
genNeq _ =
  let x = Name "x"
      y = Name "y"
      eqExpr = exprApp (exprApp (exprVar (Name "==")) (exprVar x)) (exprVar y)
      notExpr = exprApp (exprVar (Name "not")) eqExpr
  in exprLam x $ exprLam y $ notExpr

eqCaseOnX :: [(Tag, [ConstructorArg])] -> Name -> Name -> AnnotatedExpr ()
eqCaseOnX constructors x y =
  exprCase (exprVar x)
    [ Alternative (PatternConstructor tag (map (PatternVar . xArg tag) [0..n-1]))
        (eqCaseOnY tag n constructors y)
    | (tag, args) <- constructors
    , let n = length args
    ]

eqCaseOnY :: Tag -> Int -> [(Tag, [ConstructorArg])] -> Name -> AnnotatedExpr ()
eqCaseOnY tg _ constructors y =
  exprCase (exprVar y)
    [ Alternative (PatternConstructor tag2 (map (PatternVar . yArg tag2) [0..n2-1]))
        (if tag2 == tg then eqAndArgs tg n2 else boolFalse)
    | (tag2, args2) <- constructors
    , let n2 = length args2
    ]

eqAndArgs :: Tag -> Int -> AnnotatedExpr ()
eqAndArgs _tag 0 = boolTrue
eqAndArgs tag n =
  let eqPair (xa, ya) = exprApp (exprApp (exprVar (Name "==")) (exprVar xa)) (exprVar ya)
  in foldl1 exprAnd $ map eqPair $ zip (map (xArg tag) [0..n-1]) (map (yArg tag) [0..n-1])

-- === Ord ===

deriveOrd :: DataDeclaration -> InstanceDeclaration ()
deriveOrd dd =
  let constructors = declarations dd
  in InstanceDeclaration
    { instanceContext = buildContext (Name "Ord") dd
    , instanceClass = Name "Ord"
    , instanceTypes = buildInstanceTypes dd
    , instanceMethods =
        [ (Name "compare", genCompare constructors)
        , (Name ">",  genOpFromCompare ">" (Tag "GT"))
        , (Name "<",  genOpFromCompare ">" (Tag "LT"))
        , (Name ">=", genOpFromCompareNe (Tag "GT") (Tag "EQ"))
        , (Name "<=", genOpFromCompareNe (Tag "LT") (Tag "EQ"))
        ]
    }

genCompare :: [(Tag, [ConstructorArg])] -> AnnotatedExpr ()
genCompare constructors =
  let x = Name "x"
      y = Name "y"
  in exprLam x $ exprLam y $ compareCaseOnX constructors x y

genOpFromCompare :: Text -> Tag -> AnnotatedExpr ()
genOpFromCompare _ tag =
  let x = Name "x"
      y = Name "y"
  in exprLam x $ exprLam y $
       exprCase (exprApp (exprApp (exprVar (Name "compare")) (exprVar x)) (exprVar y))
         [ Alternative (PatternConstructor tag []) boolTrue
         , Alternative PatternWildcard boolFalse
         ]

genOpFromCompareNe :: Tag -> Tag -> AnnotatedExpr ()
genOpFromCompareNe tag1 tag2 =
  let x = Name "x"
      y = Name "y"
  in exprLam x $ exprLam y $
       exprCase (exprApp (exprApp (exprVar (Name "compare")) (exprVar x)) (exprVar y))
         [ Alternative (PatternConstructor tag1 []) boolTrue
         , Alternative (PatternConstructor tag2 []) boolTrue
         , Alternative PatternWildcard boolFalse
         ]

compareCaseOnX :: [(Tag, [ConstructorArg])] -> Name -> Name -> AnnotatedExpr ()
compareCaseOnX constructors x y =
  let tagOrder = Map.fromList (zip (map fst constructors) [0 :: Int ..])
  in exprCase (exprVar x)
       [ Alternative (PatternConstructor tag (map (PatternVar . xArg tag) [0..n-1]))
           (compareCaseOnY tag n constructors y tagOrder)
       | (tag, args) <- constructors
       , let n = length args
       ]

compareCaseOnY :: Tag -> Int -> [(Tag, [ConstructorArg])] -> Name -> Map.Map Tag Int -> AnnotatedExpr ()
compareCaseOnY tg _ constructors y tagOrder =
  exprCase (exprVar y)
    [ Alternative (PatternConstructor tag2 (map (PatternVar . yArg tag2) [0..n2-1]))
        (compareTwo tg tag2 n2 tagOrder)
    | (tag2, args2) <- constructors
    , let n2 = length args2
    ]

compareTwo :: Tag -> Tag -> Int -> Map.Map Tag Int -> AnnotatedExpr ()
compareTwo tag1 tag2 n2 tagOrder
  | tag1 == tag2 =
      if n2 == 0
      then ordEQ
      else compareArgs tag1 (n2 - 1)
  | otherwise =
      let i1 = tagOrder Map.! tag1
          i2 = tagOrder Map.! tag2
      in if i1 < i2 then ordLT else ordGT

compareArgs :: Tag -> Int -> AnnotatedExpr ()
compareArgs _tag (-1) = ordEQ
compareArgs tag 0 =
  exprApp (exprApp (exprVar (Name "compare")) (exprVar (xArg tag 0))) (exprVar (yArg tag 0))
compareArgs tag n =
  let a = xArg tag n
      b = yArg tag n
      first = exprApp (exprApp (exprVar (Name "compare")) (exprVar a)) (exprVar b)
      rest = compareArgs tag (n - 1)
  in exprCase first
       [ Alternative (PatternConstructor (Tag "EQ") []) rest
       , Alternative (PatternConstructor (Tag "LT") []) ordLT
       , Alternative (PatternConstructor (Tag "GT") []) ordGT
       ]

-- === Show ===

deriveShow :: DataDeclaration -> InstanceDeclaration ()
deriveShow dd =
  InstanceDeclaration
    { instanceContext = buildContext (Name "Show") dd
    , instanceClass = Name "Show"
    , instanceTypes = buildInstanceTypes dd
    , instanceMethods =
        [(Name "show", genShow (declarations dd))]
    }

genShow :: [(Tag, [ConstructorArg])] -> AnnotatedExpr ()
genShow constructors =
  let x = Name "x"
  in exprLam x $ exprCase (exprVar x)
       [ Alternative (PatternConstructor tag (map (PatternVar . showArg tag) [0..n-1]))
           (showAlt tag n)
       | (tag, args) <- constructors
       , let n = length args
       ]

showArg :: Tag -> Int -> Name
showArg (Tag t) i = Name $ "s" <> t <> T.pack (show i)

showAlt :: Tag -> Int -> AnnotatedExpr ()
showAlt (Tag tag) 0 = exprLit (LiteralString tag)
showAlt (Tag tag) n =
  let argNames = [showArg (Tag tag) i | i <- [0..n-1]]
      tagStr = exprLit (LiteralString tag)
      showns = [exprApp (exprVar (Name "show")) (exprVar a) | a <- argNames]
      parts = tagStr : [exprApp (exprApp (exprVar (Name "<>")) (exprLit (LiteralString " "))) s | s <- showns]
  in foldl1 (\l r -> exprApp (exprApp (exprVar (Name "<>")) l) r) parts

-- === Expression builders ===

exprVar :: Name -> AnnotatedExpr ()
exprVar = AnnExprVariable ()

exprLit :: Literal -> AnnotatedExpr ()
exprLit = AnnExprLiteral ()

exprApp :: AnnotatedExpr () -> AnnotatedExpr () -> AnnotatedExpr ()
exprApp = AnnExprApplication ()

exprLam :: Name -> AnnotatedExpr () -> AnnotatedExpr ()
exprLam = AnnExprLambda ()

exprCase :: AnnotatedExpr () -> [CaseAlternative ()] -> AnnotatedExpr ()
exprCase = AnnExprCase ()

boolTrue :: AnnotatedExpr ()
boolTrue = AnnExprConstructor () (Tag "True") (Arity 0)

boolFalse :: AnnotatedExpr ()
boolFalse = AnnExprConstructor () (Tag "False") (Arity 0)

ordLT :: AnnotatedExpr ()
ordLT = AnnExprConstructor () (Tag "LT") (Arity 0)

ordEQ :: AnnotatedExpr ()
ordEQ = AnnExprConstructor () (Tag "EQ") (Arity 0)

ordGT :: AnnotatedExpr ()
ordGT = AnnExprConstructor () (Tag "GT") (Arity 0)

exprAnd :: AnnotatedExpr () -> AnnotatedExpr () -> AnnotatedExpr ()
exprAnd a b =
  AnnExprCase () a
    [ Alternative (PatternConstructor (Tag "True") []) b
    , Alternative (PatternConstructor (Tag "False") []) boolFalse
    ]
