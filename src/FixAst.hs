module FixAst
  ( astFixes
  )
where

import Language
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

lookupTag :: [DataDeclaration] -> Tag -> Arity
lookupTag [] tag = error $ "Could not find constructor: " <> show tag
lookupTag (DataDeclaration [] : rest) tag = lookupTag rest tag
lookupTag (DataDeclaration ((tagX, arity):xs) : rest) tag
  | tag == tagX = arity
  | otherwise = lookupTag (DataDeclaration xs : rest) tag

fixExprArities :: [DataDeclaration] -> AnnotatedExpr a -> AnnotatedExpr a
fixExprArities _ e@(AnnExprVariable _ _) = e
fixExprArities _ e@(AnnExprInt _ _) = e
fixExprArities _ e@(AnnExprString _ _) = e
fixExprArities _ e@(AnnExprDouble _ _) = e
fixExprArities datas (AnnExprApplication a f x) = AnnExprApplication a (fixExprArities datas f) (fixExprArities datas x)
fixExprArities datas (AnnExprLet a bindings body) =
  AnnExprLet a (map (\(name, binding) -> (name, fixExprArities datas binding)) bindings) (fixExprArities datas body)
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
