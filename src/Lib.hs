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
import Control.Monad.Trans.State.Lazy (runState)

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
toLambda (name, vars, Annotated _ expr) = (name, ExprLambda vars expr)

normalizeAST :: Program -> Expr
normalizeAST program = ExprLet (map toLambda $ withoutMain program) (lookupMain program)
  where
    withoutMain :: Program -> Program
    withoutMain = filter ((\(name, _, _) -> name /= "main"))

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

          -- need to force evaluation here, so it's strict
          let
            -- typecheckingFailureHandler (TypeCheckingException pos@(SourcePos name line column) msg) = do
            typecheckingFailureHandler (TypeCheckingException msg) = do
              -- putStrLn $ "Typechecking failure at " <> sourcePosPretty pos
              -- putStrLn $ replicate (unPos line - 1) '-' <> "v"
              -- tprint $ Text.lines programText !! (unPos line - 1)
              -- putStrLn $ replicate (unPos line - 1) '-' <> "^"
              tprint msg
              exitFailure
          let
            initialTypeJudgmentState = TypeJudgmentState
                { nextTypeVarId = 0
                , bindings = Map.empty
                }
          types <- evaluate (force . infer . fst $ runState (toTypedAST program) initialTypeJudgmentState) `catch` typecheckingFailureHandler
          -- debug isDebug "TYPES" $ mapM_ (\(name, ty) -> putStrLn $ unpack name <> " : " <> show ty) types
          debug isDebug "TYPED AST" $ print types
          debug isDebug "TYPE JUDGMENTS" $ mapM_ putStrLn $ judgments types

          -- TODO: replace the below with LLVM

          let !state = compile prelude unnormalizedProgram
          -- debug isDebug "STATE" $ print state

          let !evaluated = eval state
          -- debug isDebug "EVALUATION" . tprint $ showResults evaluated

          debug isDebug "OUTPUT" $ pure ()
          strat $ showFinalResults evaluated
