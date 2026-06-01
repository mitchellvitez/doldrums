module Compile where

import Language

-- take a Program and generate LLVM IR in two passes:
--   1. lower to STG
--   2. lower to LLVM
compile :: Program a -> IO a
compile program = do
  putStrLn "pretending to compile!"
  pure . annot . head $ functions program
