{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Lib (execute, DebugFlag(..), CompileFlag(..))
import Parser (parserSpec)

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (pack, unpack, stripPrefix)
import qualified Data.Text as T
import Test.Hspec
import Control.Exception (try, SomeException)
import System.Timeout (timeout)

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension, dropExtension)

main :: IO ()
main = hspec $ do
  -- test how the parser handles pieces smaller than an entire program
  parserSpec

  -- IGNORE will skip an EXPECT or BROKEN test, as an `xit` rather than an `it`

  -- test programs that should work and produce a specified output
  -- these programs start with a comment like:
  -- -- EXPECT output
  describe "expect tests" $ do
    files <- runIO $ discoverDolFiles "test/expect"
    mapM_ mkDolTest files

  -- test programs that should not work
  -- these programs start with:
  -- -- BROKEN
  describe "broken program tests" $ do
    files <- runIO $ discoverDolFiles "test/broken"
    mapM_ mkDolTest files

  -- sanity check that the "tour.dol" program is compiling
  describe "tour" $
    it "compiles tour.dol" $ do
      content <- TIO.readFile "tour.dol"
      void $ execute content NoDebugInfo Interpreted

data TestDirective
  = ExpectDirective Text
  | IgnoreDirective
  | BrokenDirective
  | MissingDirective

data TestProgram = TestProgram
  { _name :: Text
  , _content :: Text
  , _directive :: TestDirective
  }

type ProgramContent = Text

discoverDolFiles :: FilePath -> IO [TestProgram]
discoverDolFiles dir = do
  entries <- listDirectory dir
  let dolFiles = filter ((== ".dol") . takeExtension) entries
  forM dolFiles $ \file -> do
    content <- TIO.readFile $ dir </> file
    let name = pack $ dropExtension file
        (directive, program) = parseHeader content
    pure $ TestProgram name program directive

parseHeader :: Text -> (TestDirective, ProgramContent)
parseHeader text = fromMaybe (MissingDirective, text) $ do
  (firstLine : rest) <- Just $ T.lines text
  let body = T.unlines $ dropWhile T.null rest
  pure (parseDirective firstLine, body)
  where
    parseDirective :: Text -> TestDirective
    parseDirective line = fromMaybe MissingDirective $
          ExpectDirective <$> stripPrefix "-- EXPECT " line
      <|> BrokenDirective <$  stripPrefix "-- BROKEN"  line
      <|> IgnoreDirective <$  stripPrefix "-- IGNORE"  line

mkDolTest :: TestProgram -> SpecWith ()
mkDolTest (TestProgram name _ IgnoreDirective) =
  xit (unpack name) pending
mkDolTest (TestProgram name content BrokenDirective) =
  it (unpack name) $ do
    result <- timeout 100_000 . try $ execute content NoDebugInfo Interpreted
    case result of
      Nothing -> expectationFailure "Timed out after 100ms"
      Just (Right _) -> expectationFailure "Expected an exception but program succeeded"
      Just (Left (e :: SomeException)) -> do
        TIO.putStrLn $ "  " <> pack (show e)
mkDolTest (TestProgram name content (ExpectDirective expected)) =
  it (unpack name) $ do
    result <- timeout 100_000 $ execute content NoDebugInfo Interpreted
    case result of
      Nothing -> expectationFailure "Timed out after 100ms"
      Just output -> output `shouldBe` expected
mkDolTest (TestProgram name _ MissingDirective) =
  it (unpack name) $
    expectationFailure $
      "No test directive found in " <> unpack name <> ".dol\n" <>
      "Tests must start with one of:\n" <>
      "-- EXPECT <output>\n" <>
      "-- BROKEN\n" <>
      "-- IGNORE\n"
