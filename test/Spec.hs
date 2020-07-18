{-# LANGUAGE FlexibleContexts #-}

import Language
import Parse
import Eval

import Data.Either
import Data.Text
import Data.Void
import Test.Hspec
import Text.Megaparsec

testParser :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
testParser parser input output =
  parse parser "" input `shouldBe` Right output

testParserFail :: (Show a, Eq a) => Parser a -> Text -> Expectation
testParserFail parser input =
  isLeft (parse parser "" input) `shouldBe` True

main :: IO ()
main = hspec $ do
  describe "parsing" $ do
    it "parseNumber" $ do
      testParser parseExprNumber "42" (ExprNumber 42)

    it "parseExprVariable" $ do
      testParser parseExprVariable "myVar" (ExprVariable "myVar")
      testParser parseExprVariable "var2" (ExprVariable "var2")
      -- TODO -- testParserFail parseVariable "my_var"
      --
    it "parseName" $ do
      testParser parseName "x" "x"
      testParser parseName "X" "X"
      testParserFail parseName "4eva"
      testParserFail parseName "let"
      testParserFail parseName "case"
      testParserFail parseName "in"
      testParserFail parseName "of"
      testParserFail parseName "Pack"

    it "parseExprConstructor" $ do
      testParser parseExprConstructor "Pack{0,1}" (ExprConstructor 0 1)
      testParser parseExprConstructor "Pack { 1 , 2 }" (ExprConstructor 1 2)

    it "parseDefinition" $ do
      testParser parseDefinition "x = 2;" ("x", ExprNumber 2)

    it "parseExprLet" $ do
      testParser parseExprLet "let x = 2; in x" (ExprLet [("x", ExprNumber 2)] (ExprVariable "x"))
      testParser parseExprLet "let x = 2; y = 3; in x" (ExprLet [("x", ExprNumber 2), ("y", ExprNumber 3)] (ExprVariable "x"))
      -- testParser parseExprLet "let x = 2; in let y = 3; in x" (ExprLet [("x", ExprNumber 2)] (ExprLet [("y", ExprNumber 3)] (ExprVariable "x")))

    it "parseExprCase" $ do
      testParser parseExprCase "case x of <1> -> 2; <2> -> 1;" (ExprCase (ExprVariable "x") [(1, [], ExprNumber 2), (2, [], ExprNumber 1)])

    it "parseExprLambda" $ do
      testParser parseExprLambda "\\x. x" (ExprLambda ["x"] (ExprVariable "x"))

    it "parseSupercombinator" $ do
      testParser parseSupercombinator "id x = x;" ("id", ["x"], ExprVariable "x")

    it "parseProgram" $ do
      testParser parseProgram "id x = x;\nmain = id 2;" [("id", ["x"], ExprVariable "x"), ("main", [], ExprApplication (ExprVariable "id") (ExprNumber 2))]

    it "parseExprApplication" $ do
      testParser parseExprApplication "f x" (ExprApplication (ExprVariable "f") (ExprVariable "x"))

    it "parses a simple doubling program" $ do
      testParser parseProgram "main = double 21;\ndouble x = x + x;"
        [ ("main", [], (ExprApplication (ExprVariable "double") (ExprNumber 21)))
        , ("double", ["x"], (ExprApplication (ExprApplication (ExprVariable "+") (ExprVariable "x")) (ExprVariable "x")))
        ]

    it "parses a program with many functions" $ do
      testParser parseProgram "id x = x;\nf p = (id p) * p;\ndouble n = n * 2;\nmain = f (double 4);"
        [("id",["x"],(ExprVariable "x")),("f",["p"],ExprApplication (ExprApplication (ExprVariable "*") (ExprApplication (ExprVariable "id") (ExprVariable "p"))) (ExprVariable "p")),("double",["n"],ExprApplication (ExprApplication (ExprVariable "*") (ExprVariable "n")) (ExprNumber 2)),("main",[],ExprApplication (ExprVariable "f") (ExprApplication (ExprVariable "double") (ExprNumber 4)))]

    it "parses a program with many functions" $ do
      let p = parse parseProgram "" "id x = x;\nf p = (id p) * p;\ndouble n = let b = 2; in n * b;\nmain = f (double 4);"
      case p of
        Left _ -> error "fail"
        Right p1 -> do
          let p2 = lambdaize p1
          let p3 = flattenMain p2
          p3 `shouldBe` (ExprNumber 7)

