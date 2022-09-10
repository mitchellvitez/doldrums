module Lib
 ( run
 , runDebug
 , runTest
 )
where

import Template
import Parse (parseProgram)
import Typecheck

import Control.DeepSeq (force)
import Control.Exception (throw, catch, evaluate)
import Control.Monad (when)
import Data.Text (pack, unpack, Text)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (parse, errorBundlePretty, SourcePos(..))
import Text.Megaparsec.Pos (sourcePosPretty, unPos)
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

runBase :: Text -> (Text -> IO a) -> Bool -> IO a
runBase programText strat isDebug = do
  preludeFile <- readFile "src/Prelude.dol"
  case parse parseProgram "" (pack preludeFile) of
    Left e -> error $ errorBundlePretty e
    Right prelude -> do

      debug isDebug "INPUT" $ mapM_ tprint $ Text.lines programText

      case parse parseProgram "" programText of
        Left e -> error $ errorBundlePretty e
        Right program -> do
          debug isDebug "AST" $ print program

          -- need to force evaluation here, so it's strict
          let
            typecheckingFailureHandler (TypeCheckingException pos@(SourcePos name line column) msg) = do
              putStrLn $ "Typechecking failure at " <> sourcePosPretty pos
              putStrLn $ replicate (unPos line - 1) '-' <> "v"
              tprint $ Text.lines programText !! (unPos line - 1)
              putStrLn $ replicate (unPos line - 1) '-' <> "^"
              putStrLn msg
              exitFailure
          types <- evaluate (force $ typecheck program) `catch` typecheckingFailureHandler
          debug isDebug "TYPES" $ print types

          -- TODO: replace the below with LLVM

          let !state = compile prelude program
          -- debug isDebug "STATE" $ print state

          let !evaluated = eval state
          -- debug isDebug "EVALUATION" . tprint $ showResults evaluated

          debug isDebug "OUTPUT" $ pure ()
          strat $ showFinalResults evaluated
