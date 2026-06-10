module LLVM
  ( compileAndRun
  ) where

import STG
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Monad (when)

compileModule :: StgExpr -> Text
compileModule _stg = T.unlines $ concat
  [ [ header ]
  , [ "declare %Value* @apply(%Value*, %Value*)"
    , "declare %Value* @force(%Value*)"
    , "declare %Value* @mk_int(i64)"
    , "declare i64 @puts(i8*)"
    , "declare noalias i8* @malloc(i64)"
    , ""
    ]
  , [ "define i64 @main() {"
    , "entry:"
    , "  ret i64 0"
    , "}"
    ]
  ]

header :: Text
header = T.unlines
  [ "; ModuleID = 'doldrums'"
  , "target triple = \"arm64-apple-macosx15.0.0\""
  , ""
  , "%Value = type { i64, i64, i64, i64 }"
  , ""
  ]

-- | Generates LLVM code from STG, then runs the program with help from a runtime code file, and returns its output as @Text@
compileAndRun :: StgExpr -> FilePath -> IO Text
compileAndRun stg runtimePath = do
  let llvmIR = compileModule stg
  withSystemTempDirectory "doldrums" $ \tmpDir -> do
    let llPath = tmpDir <> "/program.ll"
        objPath = tmpDir <> "/program.o"
        binPath = tmpDir <> "/program"
    TIO.writeFile llPath llvmIR

    (ec1, out1, err1) <- readProcessWithExitCode "clang"
      [ "-c", llPath, "-o", objPath ] ""
    when (ec1 /= ExitSuccess) $
      error $ "LLVM assembly failed: " <> err1 <> "\n" <> out1

    let runtimeObj = tmpDir <> "/runtime.o"
    (ec2, _, err2) <- readProcessWithExitCode "clang"
      [ "-c", runtimePath, "-o", runtimeObj ] ""
    when (ec2 /= ExitSuccess) $
      error $ "Runtime compilation failed: " <> err2

    (ec3, _, err3) <- readProcessWithExitCode "clang"
      [ objPath, runtimeObj, "-o", binPath ] ""
    when (ec3 /= ExitSuccess) $
      error $ "Linking failed: " <> err3

    (ec4, out, err4) <- readProcessWithExitCode binPath [] ""
    when (ec4 /= ExitSuccess) $
      error $ "Runtime error: " <> err4

    pure $ T.pack out
