module Lib
 ( run
 , runDebug
 , runTest
 )
where

import Language
import Graphviz
import Parse (parseProgram)
import Typecheck
import FixAst (fixAst)
import Interpret (interpret, methodEnvFromInstances)
import STG (compileStg)
import Data.Text (Text, pack, unpack)
import Data.Maybe
import qualified Data.Text as Text
import Control.Monad (when)
import System.Environment (getArgs)
import qualified Data.Map as Map
import Text.Megaparsec (parse, errorBundlePretty)

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
  let filename = fromMaybe (error "requires one filename arg") $ listToMaybe args
  input <- readFile filename
  pure $ pack input

debug :: Bool -> Text -> IO () -> IO ()
debug isDebug label action = when isDebug $ do
  putTextLn $ "\n----- " <> label <> " -----"
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
          let program = fixAst $ prelude <> unnormalizedBadAritiesProgram

          debug isDebug "AST" . print $ const () <$> program
          debug isDebug "GRAPHVIZ" . putTextLn . toGraphviz $ const () <$> program

          (types, state) <- typeInference program programText
          debug isDebug "TYPE" $ do
            let toText (Right x) = pack $ show x
                toText (Left x) = x
            putTextLn $ "main : " <> toText types
            putStrLn $ show (typeInstantiationSupply state) <> " type variables used"
            putStrLn $ "Final substitution list: " <> show (Map.toList $ typeInstantiationSubstitution state)

          -- TODO: compile this STG to LLVM to compile rather than interpreting
          let stgExpr = compileStg program
          debug isDebug "STG" $ do
            let stgExprStr = show stgExpr
                charLimit = 1000
                bigger = length stgExprStr > charLimit
            when bigger $
              putStrLn "[Showing first 1000 characters only]"
            putStrLn $ take charLimit stgExprStr
            when bigger $
              putStrLn $ "[...plus " <> show (length stgExprStr - charLimit) <> " more characters]"

          debug isDebug "OUTPUT" $ pure ()
          let
              toLambdaBinding (Function _ name args body) = (name, foldr patternToBinding body args)
              patternToBinding (PatternVar n) b = ExprLambda n b
              patternToBinding pat b = ExprLambda (Name "pat") (ExprCase (ExprVariable (Name "pat")) [Alternative pat b])
              prog = fmap (const ()) program
              topLevelBindings = map toLambdaBinding $ functions prog
              mainExpr = ExprVariable "main"
              typeMap = Map.fromList
                [ (tag, Name $ unDataType dt)
                | dtDecl <- dataDeclarations prog
                , let dt = dataType dtDecl
                , (tag, _) <- declarations dtDecl
                ]
              methodEnv = methodEnvFromInstances (instanceDeclarations prog) typeMap
              result = interpret topLevelBindings methodEnv typeMap mainExpr
          result `seq` strat result
