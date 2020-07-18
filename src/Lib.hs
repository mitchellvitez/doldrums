module Lib
 ( run
 )
where

import Template
import Parse (parseProgram)
import Text.Megaparsec (parse)
import System.Environment (getArgs)
import Control.Monad (when)

import Data.Text (pack, unpack)

tprint = putStrLn . unpack

run :: IO ()
run = do
  args <- getArgs
  when (length args /= 1) $ error "requires one filename arg"
  preludeFile <- readFile "src/Prelude.dol"
  case parse parseProgram "" (pack preludeFile) of
    Left e -> error "error parsing prelude"
    Right prelude -> do
      input <- readFile $ head args
      putStrLn "\n -- INPUT -- "
      mapM_ putStrLn $ lines input
      case parse parseProgram "" (pack input) of
        Left e -> error $ show e
        Right program -> do
          putStrLn "\n -- AST -- "
          print program
          let state = compile prelude program
          let evaluated = eval state
          let result = showResults evaluated
          putStrLn "\n -- EVALUATION -- "
          tprint result
          putStrLn "\n -- OUTPUT -- "
          tprint $ showFinalResults evaluated
