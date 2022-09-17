module Lib
 ( run
 , runDebug
 , runTest
 )
where

import Graphviz
import Parse (parseProgram)
import Typecheck
import Language
-- import Interpret
import GMachine
import Control.Exception (catch)
import Control.Monad (when)
import Data.Foldable
import Data.Text (pack, unpack, Text)
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as Map
-- import System.Exit (exitFailure)
import Text.Megaparsec (parse, errorBundlePretty, SourcePos, sourceLine, sourceColumn, unPos)
import qualified Data.Text as Text

tprint :: Text -> IO ()
tprint = putStrLn . unpack

-- | Prints its output
run :: IO ()
run = do
  programText <- getFileText
  runBase programText tprint False

-- | Returns its output, rather than printing via side effect
runTest :: Text -> IO Text
runTest programText = runBase programText pure False

-- | Prints out AST, state of the stack/heap during evaluation, etc.
runDebug :: IO ()
runDebug = do
  programText <- getFileText
  runBase programText tprint True

getFileText :: IO Text
getFileText = do
  args <- getArgs
  when (length args /= 1) $ error "requires one filename arg"
  input <- readFile $ head args
  pure $ pack input

debug :: Bool -> Text -> IO () -> IO ()
debug isDebug label action = when isDebug $ do
  tprint $ "\n -- " <> label <> " -- "
  action

lookupMain :: [Function SourcePos] -> AnnotatedExpr SourcePos
lookupMain [] = error "No main function is defined."
lookupMain (Function name _ body:rest)
  | name == "main" = body
  | otherwise = lookupMain rest

toLambda :: Function SourcePos -> (Name, AnnotatedExpr SourcePos)
toLambda (Function name args body) =
  (name, Prelude.foldr (AnnExprLambda $ annotation body) body args)

normalizeAST :: Program SourcePos -> AnnotatedExpr SourcePos
normalizeAST program = foldr (\(name, expr) -> AnnExprLet (annotation expr) name expr) (lookupMain $ functions program) lambdas
  where
    withoutMain = filter (\(Function name _ _) -> name /= "main")
    lambdas = map toLambda . withoutMain $ functions program

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
fixExprArities datas (AnnExprLet a name binding body) = AnnExprLet a name (fixExprArities datas binding) (fixExprArities datas body)
fixExprArities datas (AnnExprConstructor a tag _) = AnnExprConstructor a tag $ lookupTag datas tag
fixExprArities datas (AnnExprLambda a name expr) = AnnExprLambda a name (fixExprArities datas expr)
fixExprArities datas (AnnExprCase a expr alters) = AnnExprCase a (fixExprArities datas expr) alters

fixFunctionArities :: [DataDeclaration] -> Function a -> Function a
fixFunctionArities datas f@(Function _ _ body) =
  f { body = fixExprArities datas body }

fixArities :: Program a -> Program a
fixArities (Program funcs datas) = Program (map (fixFunctionArities datas) funcs) datas

runBase :: Text -> (Text -> IO a) -> Bool -> IO a
runBase programText strat isDebug = do
  preludeFile <- readFile "src/Prelude.dol"
  case parse parseProgram "" (pack preludeFile) of
    Left e -> error $ errorBundlePretty e
    Right prelude -> do

      debug isDebug "INPUT" $ mapM_ tprint $ Text.lines programText

      case parse parseProgram "" programText of
        Left e -> error $ errorBundlePretty e
        Right unnormalizedBadAritiesProgram -> do
          -- TODO: could instead do two parsing passes and check this at parse time
          let prelude = fixArities unnormalizedBadAritiesProgram
          let unnormalizedProgram = fixArities unnormalizedBadAritiesProgram
          let program = normalizeAST unnormalizedProgram
          debug isDebug "AST" $ print $ fmap (const void) program

          debug isDebug "GRAPHVIZ" . putStrLn . unpack $ toGraphviz $ fmap (const void) program

          (types, state) <- typeInference program `catch` typecheckingFailureHandler programText
          debug isDebug "TYPE" $ do
            let toText (Right x) = pack $ show x
                toText (Left x) = x
            tprint $ "main : " <> toText types
            putStrLn $ show (typeInstantiationSupply state) <> " type variables used"
            putStrLn $ "Final substitution list: " <> show (Map.toList $ typeInstantiationSubstitution state)

          -- an interpreter and a compiler, with different properties
          --   1. strict lambda calculus interpreter that just walks the AST
          --   2. G-machine
          --   TODO: compile to LLVM


          -- lambda calculus interpreter (strict) --
          -- debug isDebug "OUTPUT" $ pure ()
          -- let program = normalizeAST $ prelude <> unnormalizedProgram
          -- strat . interpret $ const void <$> program

          -- G machine --
          let
            toPlainExprs :: [Function SourcePos] -> [(Name, [Name], Expr)]
            toPlainExprs = map (\(Function name args body) ->
              (name, args, const void <$> body))
            constructorArities :: Map Tag Arity
            constructorArities = Map.fromList $ concatMap unDataDeclaration $ dataDeclarations (prelude <> unnormalizedProgram)
          let result = gMachineCore constructorArities $ toPlainExprs $ functions (prelude <> unnormalizedProgram)
          -- debug isDebug "EVALUATION" . tprint $ gMachineEval result
          debug isDebug "OUTPUT" $ pure ()
          strat $ gMachineOutput result

typecheckingFailureHandler :: Text -> TypeCheckingException -> IO (Either Text Type, TypeInstantiationState)
typecheckingFailureHandler programText (TypeCheckingException sourcePos msg) = do
  putStrLn $ fold
    [ "Typechecking failed at "
    , show . unPos $ sourceLine sourcePos
    , ":"
    , show . unPos $ sourceColumn sourcePos
    , " in the expression"
    ]
  let line = replicate (unPos (sourceColumn sourcePos) - 1) '-'
  putStrLn $ line <> "v"
  tprint $ Text.lines programText !! (unPos (sourceLine sourcePos) - 1)
  putStrLn $ line <> "^"
  tprint msg
  pure $ (Left "typechecking failed", TypeInstantiationState 0 Map.empty)
  -- exitFailure -- comment this out to treat typechecking as a warning
