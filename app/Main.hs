{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Control.Monad (void)
import System.Exit
import qualified Data.Text as T
import Options.Applicative

main :: IO ()
main = do
  Flags{..} <- execParser $ info (flagsParser <**> helper) fullDesc
  case helpFlag of
    ShowHelp -> showHelp exitSuccess
    NoHelp ->
      case inputFilepaths of
        [file] -> do
          programText <- T.pack <$> readFile file
          void $ execute programText debugFlag compileFlag
        _ -> showHelp exitFailure

showHelp :: IO () -> IO ()
showHelp exitStyle = do
  putStrLn $ unlines
    [ "     _       _     _                          "
    , "  __| | ___ | | __| |_ __ _   _ _ __ ___  ___ "
    , " / _` |/ _ \\| |/ _` | '__| | | | '_ ` _ \\/ __|"
    , "| (_| | (_) | | (_| | |  | |_| | | | | | \\__ \\"
    , " \\__,_|\\___/|_|\\__,_|_|   \\__,_|_| |_| |_|___/"
    , ""
    , "Doldrums - a tiny Haskell-like language"
    , ""
    , "Usage:"
    , "  cabal run doldrums -- <filename>.dol"
    , ""
    , "Flags:"
    , "  Compile (rather than interpret)"
    , "    --compile | -c"
    , "  Interpret (rather than compile)"
    , "    --interpret | -i"
    , "  Turn on debug information"
    , "    --debug | -d"
    , "  Turn off debug information"
    , "    --no-debug | -n"
    , ""
    , "Not sure where to start? Try one of these:"
    , "  cabal run doldrums -- tour.dol"
    , "  cabal run doldrums-test"
    ]
  exitStyle

data Flags = Flags
  { compileFlag :: CompileFlag
  , debugFlag :: DebugFlag
  , helpFlag :: HelpFlag
  , inputFilepaths :: [FilePath]
  }
  deriving Show

data HelpFlag = ShowHelp | NoHelp
  deriving (Show, Eq)

compileFlagParser :: Parser CompileFlag
compileFlagParser =
  flag' Compiled (long "compile" <> short 'c' <> help "Compile mode") <|>
  flag' Interpreted (long "interpret" <> short 'i' <> help "Interpret mode") <|>
  pure Interpreted

debugFlagParser :: Parser DebugFlag
debugFlagParser =
  flag' NoDebugInfo (long "no-debug" <> short 'n' <> help "No debug info") <|>
  flag' IncludeDebugInfo (long "debug" <> short 'd' <> help "Include debug info") <|>
  pure IncludeDebugInfo

helpFlagParser :: Parser HelpFlag
helpFlagParser =
  flag' ShowHelp (long "help" <> short 'h' <> help "Show help text") <|>
  pure NoHelp

flagsParser :: Parser Flags
flagsParser = Flags
  <$> compileFlagParser
  <*> debugFlagParser
  <*> helpFlagParser
  <*> many (argument str $ metavar "FILES...")
