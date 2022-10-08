module FixAst
  ( astFixes
  , singleExprForm
  )
where

import Language
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

lookupTag :: [DataDeclaration] -> Tag -> Arity
lookupTag [] tag = error $ "Could not find constructor: " <> show tag
lookupTag (DataDeclaration [] _dataType : rest) tag = lookupTag rest tag
lookupTag (DataDeclaration ((tagX, arity):xs) dataType : rest) tag
  | tag == tagX = arity
  | otherwise = lookupTag (DataDeclaration xs dataType : rest) tag

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

fixFunctionArities :: [DataDeclaration] -> Function a -> Function a
fixFunctionArities datas f@(Function _ _ _ body) =
  f { body = fixExprArities datas body }

fixArities :: Program a -> Program a
fixArities (Program funcs datas) = Program (map (fixFunctionArities datas) funcs) datas

checkUniqueFunctions :: Program a -> Program a
checkUniqueFunctions program@(Program funcs datas) =
  case findDuplicateName Set.empty $ map name funcs of
    Nothing -> program
    Just name -> error $ "Duplicate function: " <> Text.unpack name

checkUniqueConstructors :: Program a -> Program a
checkUniqueConstructors program@(Program funcs datas) =
  case findDuplicateName Set.empty . map fst $ concatMap unDataDeclaration datas of
    Nothing -> program
    Just name -> error $ "Duplicate constructor: " <> Text.unpack name

findDuplicateName :: Set Name -> [Name] -> Maybe Name
findDuplicateName seen [] = Nothing
findDuplicateName seen (x:xs) =
  if x `Set.member` seen
  then Just x
  else findDuplicateName (Set.insert x seen) xs

astFixes :: Program a -> Program a
astFixes = fixArities . checkUniqueFunctions . checkUniqueConstructors

singleExprForm :: Program a -> AnnotatedExpr a
singleExprForm program@(Program funcs _) =
  foldl' toSingle (getMainExpr program) (filter (\(Function _ name _ _) -> name /= "main") funcs)

  where
    toSingle :: AnnotatedExpr a -> Function a -> AnnotatedExpr a
    toSingle expr func = AnnExprLet (annotation expr) (name func) (toNestedLambdas func) expr

    toNestedLambdas :: Function a -> AnnotatedExpr a
    toNestedLambdas (Function annot name args body) =
      foldr (AnnExprLambda annot) body args

getMainExpr :: Program a -> AnnotatedExpr a
getMainExpr (Program [] datas) = error "Couldn't find main"
getMainExpr (Program (Function _ name _ body : restFuncs) datas) = case name of
  "main" -> body
  _ -> getMainExpr $ Program restFuncs datas
