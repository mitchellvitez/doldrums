module Lib
 ( run
 , runDebug
 , runTest
 , typecheckingFailureHandler
 )
where

import Graphviz
import Parse (parseProgram)
import Typecheck
import Language
import GMachine
import Control.Exception (catch)
import Control.Monad (when)
import Data.Foldable
import Data.Text (pack, unpack, Text)
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit (exitFailure)
import Text.Megaparsec (parse, errorBundlePretty, SourcePos, sourceLine, sourceColumn, unPos)
import qualified Data.Text as Text

putTextLn :: Text -> IO ()
putTextLn = putStrLn . unpack

-- | Prints its output
run :: IO ()
run = do
  programText <- getFileText
  runBase programText putTextLn False

-- | Returns its output, rather than printing via side effect
runTest :: Text -> IO Text
runTest programText = runBase programText pure False

-- | Prints out AST, state of the stack/heap during evaluation, etc.
runDebug :: IO ()
runDebug = do
  programText <- getFileText
  runBase programText putTextLn True

getFileText :: IO Text
getFileText = do
  args <- getArgs
  when (length args /= 1) $ error "requires one filename arg"
  input <- readFile $ head args
  pure $ pack input

debug :: Bool -> Text -> IO () -> IO ()
debug isDebug label action = when isDebug $ do
  putTextLn $ "\n -- " <> label <> " -- "
  action

-- TODO: move normalization and arity fixing to a new module
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
fixFunctionArities datas f@(Function _ _ _ body) =
  f { body = fixExprArities datas body }

fixArities :: Program a -> Program a
fixArities (Program funcs datas) = Program (map (fixFunctionArities datas) funcs) datas

runBase :: Text -> (Text -> IO a) -> Bool -> IO a
runBase programText strat isDebug = do
  preludeFile <- readFile "src/Prelude.dol"
  case parse parseProgram "" (pack preludeFile) of
    Left e -> error $ errorBundlePretty e
    Right prelude -> do

      debug isDebug "INPUT" . mapM_ putTextLn $ Text.lines programText

      case parse parseProgram "" programText of
        Left e -> error $ errorBundlePretty e
        Right unnormalizedBadAritiesProgram -> do
          let unnormalizedProgram = fixArities (prelude <> unnormalizedBadAritiesProgram)
          let unnormalizedProgramWithoutPrelude = fixArities unnormalizedBadAritiesProgram

          debug isDebug "AST" . print $ fmap (const void) unnormalizedProgramWithoutPrelude

          debug isDebug "GRAPHVIZ" . putStrLn . unpack . toGraphviz $ const void <$> unnormalizedProgramWithoutPrelude

          (types, state) <- typeInference unnormalizedProgram `catch` typecheckingFailureHandler programText
          debug isDebug "TYPE" $ do
            let toText (Right x) = pack $ show x
                toText (Left x) = x
            putTextLn $ "main : " <> toText types
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
            toPlainExprs = map (\(Function _ name args body) -> (name, args, annotatedToExpr body))

            constructorArities :: Map Tag Arity
            constructorArities = Map.fromList $ concatMap unDataDeclaration $ dataDeclarations unnormalizedProgram
          let result = gMachineCore constructorArities $ toPlainExprs $ functions unnormalizedProgram
          -- debug isDebug "EVALUATION" . putTextLn $ gMachineEval result
          -- debug isDebug "COMPILED" . putTextLn $ gMachineEval result
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
  putTextLn $ Text.lines programText !! (unPos (sourceLine sourcePos) - 1)
  putStrLn $ line <> "^"
  putTextLn msg
  pure $ (Left "typechecking failed", TypeInstantiationState 0 Map.empty)
  exitFailure -- comment this out to treat typechecking as a warning
