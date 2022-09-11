module Lib
 ( run
 , runDebug
 , runTest
 )
where

import Template
import Parse (parseProgram)
import Typecheck
import Language

import Control.DeepSeq (force)
import Control.Exception (throw, catch, evaluate)
import Control.Monad (when)
import Data.Text (pack, unpack, Text)
import System.Environment (getArgs)
import qualified Data.Map as Map
import System.Exit (exitFailure)
import Text.Megaparsec (parse, errorBundlePretty, SourcePos(..))
import Text.Megaparsec.Pos (sourcePosPretty, unPos)
import qualified Data.Text as Text
import Control.Monad.State (runState)

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

lookupMain :: Program -> Expr
lookupMain [] = error "No main function is defined."
lookupMain ((name, _, Annotated _ body):rest)
  | name == "main" = body
  | otherwise = lookupMain rest

toLambda :: TopLevelDefn -> (Name, Expr)
toLambda (name, vars, Annotated _ expr) =
  (name, Prelude.foldr ExprLambda expr vars)

normalizeAST :: Program -> Expr
normalizeAST program = foldr (\(name, expr) -> ExprLet name expr) (lookupMain program) lambdas
  where
    withoutMain = filter ((\(name, _, _) -> name /= "main"))
    lambdas = map toLambda $ withoutMain program

runBase :: Text -> (Text -> IO a) -> Bool -> IO a
runBase programText strat isDebug = do
  preludeFile <- readFile "src/Prelude.dol"
  case parse parseProgram "" (pack preludeFile) of
    Left e -> error $ errorBundlePretty e
    Right prelude -> do

      debug isDebug "INPUT" $ mapM_ tprint $ Text.lines programText

      case parse parseProgram "" programText of
        Left e -> error $ errorBundlePretty e
        Right unnormalizedProgram -> do
          let program = normalizeAST unnormalizedProgram
          debug isDebug "AST" $ print program

          let typecheckingFailureHandler (TypeCheckingException msg) =
                tprint msg >> exitFailure
          (types, state) <- typeInference program `catch` typecheckingFailureHandler
          debug isDebug "TYPE" $ do
            putStrLn $ "main : " <> (show $ (\(Right x) -> x) types)
            putStrLn $ show (typeInstantiationSupply state) <> " type variables used"
            putStrLn $ "Final substitution list: " <> show (Map.toList $ typeInstantiationSubstitution state)

          -- TODO: replace the below with LLVM

          let !state = compile prelude unnormalizedProgram
          -- debug isDebug "STATE" $ print state

          let !evaluated = eval state
          -- debug isDebug "EVALUATION" . tprint $ showResults evaluated

          debug isDebug "OUTPUT" $ pure ()
          strat $ showFinalResults evaluated
