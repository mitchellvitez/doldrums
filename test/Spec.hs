{-# LANGUAGE FlexibleContexts #-}

import Language
import Parse
import Lib (runTest)

import Control.Monad (void)
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
  parse parser "" input `shouldSatisfy` isLeft

testProgram :: Text -> Text -> Expectation
testProgram programText expectedOutput = do
  runTest programText `shouldReturn` expectedOutput

main :: IO ()
main = hspec $ do
  describe "program output" $ do
    it "constant" $ do
      testProgram "main = 3;" "3"

    it "negation" $ do
      testProgram "main = negate 3;" "-3"

    it "indirection" $ do
      testProgram "main = negate (I 3);" "-3"

    it "double negation" $ do
      testProgram "main = twice negate 3;" "3"

    it "explicit double negation" $ do
      testProgram "main = negate (negate 3);" "3"

    it "explicit double negation with $" $ do
      testProgram "main = negate $ negate 3;" "3"

    it "more complex program" $ do
      testProgram "id x = x;\nf p = (id p) * p;\ndouble n = let b = 2 in n * b;\nmain = f (double 4);" "64"

    it "regression test for recursive let and arithmetic ops" $ do
      testProgram "three = 3;\nfour = ((1 + 6 - 2) * 4) / 5;\n\npair x y f = f x y;\nfst p = p K;\nsnd p = p K1;\nf x y = let a = pair x b, b = pair y a in fst (snd (snd (snd a)));main = f three four;" "4"

    it "recursive functions like factorial" $ do
      testProgram "fac n = if (n == 0) 1 (n * fac (n-1)); main = fac 3;" "6"

    it "ap $" $ do
      testProgram "main = square $ addOne $ double 3; square x = x*x; addOne x = x + 1; double x = x + x;" "49"

  describe "parsing" $ do
    it "parseNumber" $ do
      testParser parseExprLiteral "42" (ExprLiteral (ValueInt 42))

    it "parseExprVariable" $ do
      testParser parseExprVariable "myVar" (ExprVariable "myVar")
      testParser parseExprVariable "var2" (ExprVariable "var2")
      testParser parseExprVariable "my_var" (ExprVariable "my_var")
      testParserFail parseExprVariable "'var'"

    it "parseName" $ do
      testParser parseName "x" "x"
      testParser parseName "X" "X"
      testParser parseName "letMeIn" "letMeIn"
      testParser parseName "innocent" "innocent"
      testParser parseName "ofCourse" "ofCourse"
      testParser parseName "Packed" "Packed"
      testParserFail parseName "4eva"
      testParserFail parseName "let"
      testParserFail parseName "in"
      testParserFail parseName "of"
      testParserFail parseName "Pack"

    it "$" $ do
      testParser parseProgram "main = negate $ negate 3;" [("main",[],ExprApplication (ExprApplication (ExprVariable "$") (ExprVariable "negate")) (ExprApplication (ExprVariable "negate") (ExprLiteral (ValueInt 3))))]

    it "parseExprConstructor" $ do
      testParser parseExprConstructor "Pack{0,1}" (ExprConstructor 0 1)
      testParser parseExprConstructor "Pack { 1 , 2 }" (ExprConstructor 1 2)

    it "parseDefinition" $ do
      testParser parseDefinition "x = 2" ("x", ExprLiteral (ValueInt 2))

    it "parseExprLet" $ do
      testParser parseExprLet "let x = 2 in x" (ExprLet [("x", ExprLiteral (ValueInt 2))] (ExprVariable "x"))
      testParser parseExprLet "let x = 2, y = 3 in x" (ExprLet [("x", ExprLiteral (ValueInt 2)), ("y", ExprLiteral (ValueInt 3))] (ExprVariable "x"))
      testParser parseExprLet "let x = 2 in let y = 3 in x" (ExprLet [("x", ExprLiteral (ValueInt 2))] (ExprLet [("y", ExprLiteral (ValueInt 3))] (ExprVariable "x")))

    it "parseExprLambda" $ do
      testParser parseExprLambda "\\x. x" (ExprLambda ["x"] (ExprVariable "x"))

    it "parseSupercombinator" $ do
      testParser parseSupercombinator "id x = x;" ("id", ["x"], ExprVariable "x")

    it "parseProgram" $ do
      testParser parseProgram "id x = x;\nmain = id 2;" [("id", ["x"], ExprVariable "x"), ("main", [], ExprApplication (ExprVariable "id") (ExprLiteral (ValueInt 2)))]

    it "parseExprApplication" $ do
      testParser parseExprApplication "f x" (ExprApplication (ExprVariable "f") (ExprVariable "x"))

    it "parses a simple doubling program" $ do
      testParser parseProgram "main = double 21;\ndouble x = x + x;"
        [ ("main", [], (ExprApplication (ExprVariable "double") (ExprLiteral (ValueInt 21))))
        , ("double", ["x"], (ExprApplication (ExprApplication (ExprVariable "+") (ExprVariable "x")) (ExprVariable "x")))
        ]

    it "parses a program with many functions" $ do
      testParser parseProgram "id x = x;\nf p = (id p) * p;\ndouble n = n * 2;\nmain = f (double 4);"
        [("id",["x"],(ExprVariable "x")),("f",["p"],ExprApplication (ExprApplication (ExprVariable "*") (ExprApplication (ExprVariable "id") (ExprVariable "p"))) (ExprVariable "p")),("double",["n"],ExprApplication (ExprApplication (ExprVariable "*") (ExprVariable "n")) (ExprLiteral (ValueInt 2))),("main",[],ExprApplication (ExprVariable "f") (ExprApplication (ExprVariable "double") (ExprLiteral (ValueInt 4))))]
