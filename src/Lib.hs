module Lib
 ( run
 , runDebug
 , runTest
 )
where

import Graphviz
import Parse (parseProgram)
import Typecheck
import FixAst
import Language
import GMachine
import Control.Monad (when)
import Data.Text (pack, unpack, Text)
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec (parse, errorBundlePretty, SourcePos)
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
          let program = astFixes (prelude <> unnormalizedBadAritiesProgram)
          let programWithoutPrelude = astFixes unnormalizedBadAritiesProgram

          debug isDebug "AST" . print $ fmap (const ()) unnormalizedBadAritiesProgram
          debug isDebug "GRAPHVIZ" . putStrLn . unpack . toGraphviz $ const void <$> programWithoutPrelude

          (types, state) <- typeInference program programText
          debug isDebug "TYPE" $ do
            let toText (Right x) = pack $ show x
                toText (Left x) = x
            putTextLn $ "main : " <> toText types
            putStrLn $ show (typeInstantiationSupply state) <> " type variables used"
            putStrLn $ "Final substitution list: " <> show (Map.toList $ typeInstantiationSubstitution state)

          let
            toPlainExprs :: [Function SourcePos] -> [(Name, [Name], Expr)]
            toPlainExprs = map (\(Function _ name args body) -> (name, args, annotatedToExpr body))

            constructorArities :: Map Tag Arity
            constructorArities = Map.fromList $ concatMap unDataDeclaration $ dataDeclarations program
          let result = gMachineCore constructorArities $ toPlainExprs $ functions program
          -- debug isDebug "EVALUATION" . putTextLn $ gMachineEval result
          debug isDebug "OUTPUT" $ pure ()
          strat $ gMachineOutput result
