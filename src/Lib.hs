module Lib
 ( run
 , runDebug
 , runTest
 )
where

import Template
import Parse (parseProgram)
import Typecheck

import Control.Exception (throw)
import Control.Monad (when)
import Data.Text (pack, unpack, Text)
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty)
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

runBase :: Text -> (Text -> IO a) -> Bool -> IO a
runBase programText strat debug = do
  preludeFile <- readFile "src/Prelude.dol"
  case parse parseProgram "" (pack preludeFile) of
    Left e -> error $ errorBundlePretty e
    Right prelude -> do

      when debug $ do
        putStrLn "\n -- INPUT -- "
        mapM_ tprint $ Text.lines programText

      case parse parseProgram "" programText of
        Left e -> error $ errorBundlePretty e
        Right program -> do

          when debug $ do
            putStrLn "\n -- AST -- "
            print program

          -- need to force evaluation here, so it's strict
          let !types = typecheck program

          when debug $ do
            putStrLn "\n -- TYPES -- "
            print types

          let state = compile prelude program
          let evaluated = eval state
          let result = showResults evaluated

          when debug $ do
            putStrLn "\n -- EVALUATION -- "
            tprint result
            putStrLn "\n -- OUTPUT -- "

          strat $ showFinalResults evaluated
