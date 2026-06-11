{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Control.Exception (try, SomeException, displayException)
import Control.Monad (void)
import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit
import System.IO

data ReplState = ReplState
  { prelude :: Text
  , loadedFile :: Maybe (FilePath, Text)
  }

main :: IO ()
main = do
  prelude <- T.pack <$> readFile "src/Prelude.dol"
  void $ runStateT repl $ ReplState prelude Nothing

showPrompt :: REPL ()
showPrompt = liftIO $ do
  putStr prompt
  hFlush stdout

prompt :: String
prompt = "\ESC[34mdoldrums\ESC[0m\ESC[1;32m λ \ESC[0m"

type REPL = StateT ReplState IO

repl :: REPL ()
repl = do
  showPrompt
  -- read
  input <- liftIO getLine
  -- eval, print
  case input of
    (':':line) ->
      case words line of
        (command:rest) -> handleCommand command $ unwords rest
        cmd -> unknownCommand $ unwords cmd
    expr -> evalExpr expr
  -- loop
  repl

handleCommand :: String -> String -> REPL ()
handleCommand command rest = case command of
  "h"; "help" -> liftIO $ putStr helpMessage
  "l"; "load" -> case words rest of
    [filePath] -> do
      fileText <- liftIO $ T.pack <$> readFile filePath
      modify (\s -> s { loadedFile = Just (filePath, fileText)})
      liftIO . putStrLn $ "Loaded " <> filePath
    _ -> liftIO $ putStrLn ":load takes a single filename arg"
  "r"; "reload" -> do
      ReplState{..} <- get
      liftIO $ case loadedFile of
        Nothing -> putStrLn "No file to reload"
        Just (path, _) -> putStrLn $ "Reloaded " <> path
  -- "t"; "type" -> _type
  "q"; "quit" -> liftIO $ putStrLn "Leaving Doldrums REPL" >> exitSuccess
  cmd -> unknownCommand cmd

unknownCommand :: String -> REPL ()
unknownCommand cmd = liftIO $ do
  putStrLn $ "Unknown command :" <> cmd
  putStrLn ":help to list valid commands"

helpMessage :: String
helpMessage = unlines
  [ "Doldrums REPL commands"
  , ":h  :help    show this help message"
  , ":l  :load    load a source file"
  , ":r  :reload  reload the current source file"
  , ":t  :type    examine the type of an expression"
  , ":q  :quit    exit the REPL"
  ]

evalExpr :: String -> REPL ()
evalExpr expr = do
  result <- liftIO . try $ execute ("main = print $ " <> T.pack expr) NoDebugInfo Interpreted
  case result of
    Left (e :: SomeException) -> liftIO $ do
      putStrLn "An exception occurred:"
      putStrLn $ displayException e
    Right _ -> pure ()

