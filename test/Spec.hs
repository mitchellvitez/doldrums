{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Language
import Parse
import Lib (runTest)
import Typecheck

import Control.Monad (void)
import Data.Either
import Data.Text
import Data.Void
import Test.Hspec
import Text.Megaparsec

testParser :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
testParser parser input output =
  parse parser "" input `shouldBe` Right output

testProgramParser :: Parser Program -> Text -> [(Name, [Name], Expr)] -> Expectation
testProgramParser parser input output =
  fmap ignoreAnnotations (parse parser "" input) `shouldBe` Right output
  where
    ignoreAnnotations =
      Prelude.map (\(name, args, expr) -> (name, args, const Language.void <$> expr))

testParserFail :: (Show a, Eq a) => Parser a -> Text -> Expectation
testParserFail parser input =
  parse parser "" input `shouldSatisfy` isLeft

testProgram :: Text -> Text -> Expectation
testProgram programText expectedOutput = do
  runTest programText `shouldReturn` expectedOutput

testProgramException :: Text -> Expectation
testProgramException programText = do
  runTest programText `shouldThrow` anyException

main :: IO ()
main = hspec $ do
  describe "typechecking" $ do
    it "num plus string - parses" $ do
      testProgramParser parseProgram "main = 1 + \"hello\";" [("main", [], (ExprApplication (ExprApplication (ExprVariable "+") (ExprInt 1))) (ExprString "hello"))]

    it "num plus string" $ do
      testProgramException "main = 1 + \"hello\";"

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

    it "mutually recursive functions" $ do
      testProgram "f n = if (n == 0) 1 (g - 1); g n = f (n - 1); main = f 3;" "1"

    it "any top-level order" $ do
      testProgram "main = b; b = c; a = 7; c = a;" "7"

    it "ap $" $ do
      testProgram "main = square $ addOne $ double 3; square x = x*x; addOne x = x + 1; double x = x + x;" "49"

  describe "parsing" $ do
    it "parseNumber" $ do
      testParser parseExprLiteral "42" (ExprInt 42)

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
      testProgramParser parseProgram "main = negate $ negate 3;" [("main",[],ExprApplication (ExprVariable "negate") (ExprApplication (ExprVariable "negate") (ExprInt 3)))]

    it "parseExprConstructor" $ do
      testParser parseExprConstructor "Pack{0,1}" (ExprConstructor 0 1)
      testParser parseExprConstructor "Pack { 1 , 2 }" (ExprConstructor 1 2)

    it "parseDefinition" $ do
      testParser parseDefinition "x = 2" ("x", ExprInt 2)

    it "parseExprLet" $ do
      testParser parseExprLet "let x = 2 in x" (ExprLet "x" (ExprInt 2) (ExprVariable "x"))
      testParser parseExprLet "let x = 2, y = 3 in x" (ExprLet "x" (ExprInt 2) (ExprLet "y" (ExprInt 3) (ExprVariable "x")))
      testParser parseExprLet "let x = 2 in let y = 3 in x" (ExprLet "x" (ExprInt 2) (ExprLet "y" (ExprInt 3) (ExprVariable "x")))

    fit "parseExprCase" $ do
      testParser parseExprCase "case c of 1 -> 2, 2 -> 3" (ExprCase (ExprVariable "c") [(1, [], ExprInt 2), (2, [], ExprInt 3)])

    it "parseExprLambda" $ do
      testParser parseExprLambda "\\x. x" (ExprLambda "x" (ExprVariable "x"))

    -- it "parseTopLevelDefn" $ do
    --   testTopLevelDefnParser parseTopLevelDefn "id x = x;" ("id", ["x"], ExprVariable "x")

    it "parseProgram" $ do
      testProgramParser parseProgram "id x = x;\nmain = id 2;" [("id", ["x"], ExprVariable "x"), ("main", [], ExprApplication (ExprVariable "id") (ExprInt 2))]

    it "parseExprApplication" $ do
      testParser parseExprApplication "f x" (ExprApplication (ExprVariable "f") (ExprVariable "x"))

    it "parses a simple doubling program" $ do
      testProgramParser parseProgram "main = double 21;\ndouble x = x + x;"
        [ ("main", [], (ExprApplication (ExprVariable "double") (ExprInt 21)))
        , ("double", ["x"], (ExprApplication (ExprApplication (ExprVariable "+") (ExprVariable "x")) (ExprVariable "x")))
        ]

    it "parses a program with many functions" $ do
      testProgramParser parseProgram "id x = x;\nf p = (id p) * p;\ndouble n = n * 2;\nmain = f (double 4);"
        [("id",["x"],(ExprVariable "x")),("f",["p"],ExprApplication (ExprApplication (ExprVariable "*") (ExprApplication (ExprVariable "id") (ExprVariable "p"))) (ExprVariable "p")),("double",["n"],ExprApplication (ExprApplication (ExprVariable "*") (ExprVariable "n")) (ExprInt 2)),("main",[],ExprApplication (ExprVariable "f") (ExprApplication (ExprVariable "double") (ExprInt 4)))]
