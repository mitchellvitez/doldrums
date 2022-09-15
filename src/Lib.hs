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
-- import TemplateInstantiation
import GMachine

import Control.Exception (catch)
import Control.Monad (when)
import Data.Foldable
import Data.Text (pack, unpack, Text)
import System.Environment (getArgs)
import qualified Data.Map as Map
import System.Exit (exitFailure)
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

lookupMain :: Program -> AnnotatedExpr SourcePos
lookupMain [] = error "No main function is defined."
lookupMain ((name, _, body):rest)
  | name == "main" = body
  | otherwise = lookupMain rest

toLambda :: TopLevelDefn -> (Name, AnnotatedExpr SourcePos)
toLambda (name, vars, expr) =
  (name, Prelude.foldr (AnnExprLambda $ annotation expr) expr vars)

normalizeAST :: Program -> AnnotatedExpr SourcePos
normalizeAST program = foldr (\(name, expr) -> AnnExprLet (annotation expr) name expr) (lookupMain program) lambdas
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
          debug isDebug "AST" $ print $ fmap (const void) program

          debug isDebug "GRAPHVIZ" . putStrLn . unpack $ toGraphviz $ fmap (const void) program

          let typecheckingFailureHandler (TypeCheckingException sourcePos msg) = do
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
                exitFailure
          (types, state) <- typeInference program `catch` typecheckingFailureHandler
          debug isDebug "TYPE" $ do
            putStrLn $ "main : " <> (show $ (\(Right x) -> x) types)
            putStrLn $ show (typeInstantiationSupply state) <> " type variables used"
            putStrLn $ "Final substitution list: " <> show (Map.toList $ typeInstantiationSubstitution state)

          -- many different interpreters/compilers exist with different properties
          --   1. strict lambda calculus interpreter that just walks the AST
          --   2. lazy template instantiation interpreter
          --   3. G-machine
          -- TODO: compile to LLVM

          -- lambda calculus interpreter (strict) --
          -- debug isDebug "OUTPUT" $ pure ()
          -- let program = normalizeAST $ prelude <> unnormalizedProgram
          -- strat . interpret $ const void <$> program

          -- template instantiation --
          -- let !state = toInitialState prelude unnormalizedProgram
          -- debug isDebug "STATE" $ print state

          -- let !evaluated = eval state
          -- debug isDebug "EVALUATION" . tprint $ showResults evaluated

          -- debug isDebug "OUTPUT" $ pure ()
          -- strat $ showFinalResults evaluated

          -- G machine --
          debug isDebug "OUTPUT" $ pure ()
          strat . gMachine $ map (\(a, b, c) -> (a, b, const void <$> c)) unnormalizedProgram
