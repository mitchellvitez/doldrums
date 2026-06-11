{-# LANGUAGE RecordWildCards #-}

module Lib
 ( execute
 , DebugFlag(..)
 , CompileFlag(..)
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
import qualified Data.Text as Text
import Control.Monad (when, unless)
import qualified Data.Map as Map
import Text.Megaparsec (parse, errorBundlePretty)
import System.Directory ()
import System.Exit (exitFailure)

putTextLn :: Text -> IO ()
putTextLn = putStrLn . unpack

debug :: DebugFlag -> Text -> IO () -> IO ()
debug debugFlag label action = when (debugFlag == IncludeDebugInfo) $ do
  putTextLn $ "\n----- " <> label <> " -----"
  action

data DebugFlag = IncludeDebugInfo | NoDebugInfo
  deriving (Eq, Show)

data CompileFlag = Compiled | Interpreted
  deriving (Eq, Show)

execute :: Text -> DebugFlag -> CompileFlag -> IO Text
execute programText debugFlag compileFlag = do
  preludeFile <- readFile "src/Prelude.dol"

  case parse parseProgram "" (pack preludeFile) of
    Left e -> error $ errorBundlePretty e
    Right prelude -> do
      debug debugFlag "INPUT" . mapM_ putTextLn $ Text.lines programText

      case parse parseProgram "" programText of
        Left e -> error $ errorBundlePretty e
        Right unnormalizedBadAritiesProgram -> do
          let program = fixAst $ prelude <> unnormalizedBadAritiesProgram

          debug debugFlag "AST" . print $ const () <$> program
          debug debugFlag "GRAPHVIZ" . putTextLn . toGraphviz $ const () <$> program

          (types, state) <- typeInference program programText
          debug debugFlag "TYPE" $ do
            let toText (Right x) = pack $ show x
                toText (Left x) = x
            putTextLn $ "main : " <> toText types
            putStrLn $ show (typeInstantiationSupply state) <> " type variables used"
            putStrLn $ "Final substitution list: " <> show (Map.toList $ typeInstantiationSubstitution state)

          let holes = typedHoles state
          unless (null holes) $ do
            reportHoles programText (typeInstantiationSubstitution state) holes
            exitFailure

          let stgExpr = compileStg program
          debug debugFlag "STG" $ do
            let stgExprStr = show stgExpr
                charLimit = 1000
                bigger = length stgExprStr > charLimit
            when bigger $
              putStrLn "[Showing first 1000 characters only]"
            putStrLn $ take charLimit stgExprStr
            when bigger $
              putStrLn $ "[...plus " <> show (length stgExprStr - charLimit) <> " more characters]"

          debug debugFlag "OUTPUT" $ pure ()
          case compileFlag of
            Compiled -> do
              error "Compilation to LLVM IR is still a work in progress"
              -- TODO: create a runtime and do LLVM IR generation
              -- result <- compileAndRun stgExpr runtimePath
              -- strat result

            Interpreted -> do
              let
                  toLambdaBinding Function{..} = (name, foldr (patternToLambda ()) body args)
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
              interpret topLevelBindings methodEnv typeMap mainExpr
