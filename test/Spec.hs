{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Text.RawString.QQ (r)

-- TODO: fix all `xit`s in this file

testParser :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
testParser parser input output =
  parse parser "" input `shouldBe` Right output

testProgramParser :: Parser (Program SourcePos) -> Text -> Program Void -> Expectation
testProgramParser parser input output =
  (ignoreAnnotations <$> parse parser "" input) `shouldBe` Right output
  where
    ignoreAnnotations (Program funcs datas) = Program funcs' datas
      where funcs' = Prelude.map (\(Function name args expr) -> Function name args (const Language.void <$> expr)) funcs

testParserFail :: (Show a, Eq a) => Parser a -> Text -> Expectation
testParserFail parser input =
  parse parser "" input `shouldSatisfy` isLeft

testProgram :: Text -> Text -> Expectation
testProgram expectedOutput programText = do
  runTest programText `shouldReturn` expectedOutput

testProgramException :: Text -> Expectation
testProgramException programText = do
  runTest programText `shouldThrow` anyException

main :: IO ()
main = hspec $ do
  describe "typechecking" $ do
    xit "num plus string - parses" $ do
      testProgramParser parseProgram "main = 1 + \"hello\";" (Program [Function "main" [] ((ExprApplication (ExprApplication (ExprVariable "+") (ExprInt 1))) (ExprString "hello"))] [])

    xit "num plus string" $ do
      testProgramException "main = 1 + \"hello\";"

  describe "program output" $ do
    it "constant" $ do
      testProgram "3" "main = 3;"

    it "negation" $ do
      testProgram "-3" "main = negate 3;"

    xit "indirection" $ do
      testProgram "-3" "main = negate (I 3);"

    xit "double negation" $ do
      testProgram "3" "main = twice negate 3;"

    it "explicit double negation" $ do
      testProgram "3" "main = negate (negate 3);"

    it "explicit double negation with $" $ do
      testProgram "3" "main = negate $ negate 3;"

    it "more complex program" $ do
      testProgram  "64""id x = x;\nf p = (id p) * p;\ndouble n = let b = 2 in n * b;\nmain = f (double 4);"

    xit "regression test for recursive let and arithmetic ops" $ do
      testProgram "4" "three = 3;\nfour = ((1 + 6 - 2) * 4) / 5;\n\npair x y f = f x y;\nfst p = p K;\nsnd p = p K1;\nf x y = let a = pair x b, b = pair y a in fst (snd (snd (snd a)));main = f three four;"

    xit "recursive functions like factorial" $ do
      testProgram "6" "fac n = if (n == 0) 1 (n * fac (n-1)); main = fac 3;"

    xit "mutually recursive functions" $ do
      testProgram "1" "f n = if (n == 0) 1 (g (n - 1)); g n = f (n - 1); main = f 3;"

    it "any top-level order" $ do
      testProgram "7" "main = b; b = c; a = 7; c = a;"

    it "ap $" $ do
      testProgram "49" "main = square $ addOne $ double 3; square x = x*x; addOne x = x + 1; double x = x + x;"

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
      testParserFail parseName "X"
      testParser parseName "letMeIn" "letMeIn"
      testParser parseName "innocent" "innocent"
      testParser parseName "ofCourse" "ofCourse"
      testParser parseName "pack" "pack"
      testParserFail parseName "Packed"
      testParserFail parseName "4eva"
      testParserFail parseName "let"
      testParserFail parseName "in"
      testParserFail parseName "of"
      testParserFail parseName "Pack"

    it "$" $ do
      testProgramParser parseProgram [r|
main = negate $ negate 3;
|]
        (Program [Function "main" [] (ExprApplication (ExprVariable "negate") (ExprApplication (ExprVariable "negate") (ExprInt 3)))] [])

    it "parseExprConstructor" $ do
      testParser parseExprConstructor "True" (ExprConstructor "True" (-1))
      testParser parseExprConstructor "False" (ExprConstructor "False" (-1))

    it "parseDefinition" $ do
      testParser parseDefinition "x = 2" ("x", ExprInt 2)

    it "parseExprLet" $ do
      testParser parseExprLet "let x = 2 in x" (ExprLet "x" (ExprInt 2) (ExprVariable "x"))
      testParser parseExprLet "let x = 2, y = 3 in x" (ExprLet "x" (ExprInt 2) (ExprLet "y" (ExprInt 3) (ExprVariable "x")))
      testParser parseExprLet "let x = 2 in let y = 3 in x" (ExprLet "x" (ExprInt 2) (ExprLet "y" (ExprInt 3) (ExprVariable "x")))

    -- it "parseExprCase" $ do
    --   testParser parseExprCase "case c of 1 -> 2, 2 -> 3" (ExprCase (ExprVariable "c") [("False", [], ExprInt 2), ("True", [], ExprInt 3)])

    it "parseExprLambda" $ do
      testParser parseExprLambda "\\x. x" (ExprLambda "x" (ExprVariable "x"))

    -- it "parseTopLevelDefn" $ do
    --   testTopLevelDefnParser parseTopLevelDefn "id x = x;" ("id", ["x"], ExprVariable "x")

    xit "parseProgram" $ do
      testProgramParser parseProgram [r|
id x = x;
main = id 2;
|]
        (Program [Function "id"  ["x"] (ExprVariable "x"), Function "main" [] (ExprApplication (ExprVariable "id") (ExprInt 2))] [])

    xit "parseExprApplication" $ do
      testParser parseExprApplication "f x" (ExprApplication (ExprVariable "f") (ExprVariable "x"))

    xit "parses a simple doubling program" $ do
      testProgramParser parseProgram [r|
main = double 21;
double x = x + x;
|]
        (Program [ Function "main" [] (ExprApplication (ExprVariable "double") (ExprInt 21))
        , Function "double" ["x"] (ExprApplication (ExprApplication (ExprVariable "+") (ExprVariable "x")) (ExprVariable "x"))
        ] [])

    xit "parses a program with many functions" $ do
      testProgramParser parseProgram [r|
id x = x;
f p = (id p) * p;
double n = n * 2;
main = f (double 4);
|]
        (Program [Function "id" ["x"] (ExprVariable "x"), Function "f" ["p"] (ExprApplication (ExprApplication (ExprVariable "*") (ExprApplication (ExprVariable "id") (ExprVariable "p"))) (ExprVariable "p")), Function "double" ["n"] (ExprApplication (ExprApplication (ExprVariable "*") (ExprVariable "n")) (ExprInt 2)), Function "main" [] (ExprApplication (ExprVariable "f") (ExprApplication (ExprVariable "double") (ExprInt 4)))] [])


  describe "test full programs - " $ do
    it "hello world" $ do
      testProgram "\"Hello, world!\"" [r|
main = "Hello, world!";
|]

    it "fibonacci" $ do
      testProgram "34" [r|
main = fib 8;

fib n =
  if (n == 0) 1 $
  if (n == 1) 1 $
  fib (n - 1) + fib (n - 2);
|]

    it "pair of arithmetic ops" $ do
      testProgram "7" [r|
three = if (2 == 3 || 3 < 4) 3 7;
four = ((1 + 6 - 2) * 4) / 5;

data Pair 2;
first p = case p of Pair a b -> a;
second p = case p of Pair a b -> b;
id x = x;

f x y = first $ Pair (id x) (second $ Pair 3 y);

main =
  f three (id four);
|]

    it "factorial" $ do
      testProgram "720" [r|
main = fac $ 5 + 1;
fac n = if (n == 0) 1 $ n * fac (n-1);
|]

    it "bool" $ do
      testProgram "3" [r|
data True 0 | False 0;

myIf c t f = case c of
  True -> t,
  False -> f;

main = myIf (1 < 2) 3 4;
|]

    it "length of list" $ do
      testProgram "2" [r|
data Nil 0 | Cons 2;

length list = case list of
  Nil -> 0,
  Cons x xs -> 1 + length xs;

main = length $ Cons 1 $ Cons 2 Nil;
|]

    it "maybe withDefault" $ do
      testProgram "9" [r|
data Nothing 0 | Just 1;

withDefault maybe default = case maybe of
  Nothing -> default,
  Just x -> x;

main = withDefault Nothing 7 + withDefault (Just 2) 4;
|]
