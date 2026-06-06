{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser
  ( parserSpec
  , testProgramParser
  )
where

import Language
import Parse
import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ (r)
import Data.Either
import Data.Text (Text)

testParser :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
testParser parser input output =
  parse parser "" input `shouldBe` Right output

testProgramParser :: Parser (Program SourcePos) -> Text -> Program () -> Expectation
testProgramParser parser input output =
  (ignoreAnnotations <$> parse parser "" input) `shouldBe` Right output
  where
    ignoreAnnotations (Program funcs datas sigs _ _) = Program funcs' datas sigs [] []
      where funcs' = Prelude.map (\(Function annot name args expr) -> Function () name args (const () <$> expr)) funcs

testParserFail :: (Show a, Eq a) => Parser a -> Text -> Expectation
testParserFail parser input =
  parse parser "" input `shouldSatisfy` isLeft

parserSpec :: Spec
parserSpec = describe "parsing" $ do
    it "parseInt" $ do
      testParser parseExprLiteral "42" (ExprLiteral (LiteralInt 42))
      testParser parseExprLiteral "0" (ExprLiteral (LiteralInt 0))
      testParser parseExprLiteral "99999999999999999999999" (ExprLiteral (LiteralInt 99999999999999999999999))

    it "parseDouble" $ do
      testParser parseExprLiteral "3.14" (ExprLiteral (LiteralDouble 3.14))
      testParser parseExprLiteral "0.15" (ExprLiteral (LiteralDouble 0.15))

    it "parseNegativeInt" $ do
      testParser parseExprLiteral "-42" (ExprLiteral (LiteralInt (-42)))
      testParser parseExprLiteral "-0" (ExprLiteral (LiteralInt 0))

    it "parseNegativeDouble" $ do
      testParser parseExprLiteral "-3.14" (ExprLiteral (LiteralDouble (-3.14)))
      testParser parseExprLiteral "-0.15" (ExprLiteral (LiteralDouble (-0.15)))

    it "rejects - with space as literal" $ do
      testParserFail parseExprLiteral "- 7"
      testParserFail parseExprLiteral "- 3.14"

    it "parseExpr: f -7 is App f (-7), not subtraction" $ do
      testParser parseExpr "f -7"
        (ExprApplication (ExprVariable "f") (ExprLiteral (LiteralInt (-7))))

    it "parseExpr: f - 7 is binary subtraction" $ do
      testParser parseExpr "f - 7"
        (ExprApplication (ExprApplication (ExprVariable "-") (ExprVariable "f")) (ExprLiteral (LiteralInt 7)))

    it "parseString" $ do
      testParser parseExprLiteral "\"hello\"" (ExprLiteral (LiteralString "hello"))
      testParser parseExprLiteral "\"\"" (ExprLiteral (LiteralString ""))
      testParser parseExprLiteral "\"你好\"" (ExprLiteral (LiteralString "你好"))

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

    it "parseExprParenthesized" $ do
      testParser parseExpr "(42)" (ExprLiteral (LiteralInt 42))
      testParser parseExpr "((42))" (ExprLiteral (LiteralInt 42))

    it "$" $ do
      testProgramParser parseProgram [r|
main = negate $ negate 3
|]
        (Program [Function () "main" [] (ExprApplication (ExprVariable "negate") (ExprApplication (ExprVariable "negate") (ExprLiteral (LiteralInt 3))))] [] [] [] [])

    it "parseExprConstructor" $ do
      testParser parseExprConstructor "True" (ExprConstructor "True" (-1))
      testParser parseExprConstructor "False" (ExprConstructor "False" (-1))

    it "parseDataDeclaration" $ do
      testParser parseDataDeclaration "data Bool = True | False"
        (DataDeclaration [("True", []), ("False", [])] (DataType "Bool") [])
      testParser parseDataDeclaration "data Maybe a = Nothing | Just a"
        (DataDeclaration [("Nothing", []), ("Just", [TypeRefVar "a"])] (DataType "Maybe") [Name "a"])

    it "parseDefinition" $ do
      testParser parseDefinition "x = 2" ("x", ExprLiteral (LiteralInt 2))

    it "parseExprCase" $ do
      testParser parseExprCase "case maybe of\n  Nothing -> 0\n  Just x -> x" (ExprCase (ExprVariable "maybe") [Alternative (PatternConstructor "Nothing" []) (ExprLiteral (LiteralInt 0)), Alternative (PatternConstructor "Just" [PatternVar "x"]) (ExprVariable "x")])

    it "parseExprLet" $ do
      testParser parseExprLet "let\n  x = 2\nin x" $ ExprLet [(Name "x", ExprLiteral (LiteralInt 2))] (ExprVariable "x")
      -- multiple
      testParser parseExprLet "let\n  x = 2\n  y = 3\nin x" $ ExprLet [(Name "x", ExprLiteral (LiteralInt 2)), (Name "y", ExprLiteral (LiteralInt 3))] (ExprVariable "x")
      -- nested
      testParser parseExprLet "let\n  x = 2\nin\nlet\n  y = 3\nin x" $ ExprLet [(Name "x", ExprLiteral (LiteralInt 2))] (ExprLet [(Name "y", ExprLiteral (LiteralInt 3))] (ExprVariable "x"))

    it "parseExprLambda" $ do
      testParser parseExprLambda "\\x -> x" (ExprLambda "x" (ExprVariable "x"))
      testParser parseExprLambda "\\x y -> x" (ExprLambda "x" (ExprLambda "y" (ExprVariable "x")))

    it "operator precedence (* over +)" $ do
      testParser parseExpr "1 + 2 * 3"
        (ExprApplication (ExprApplication (ExprVariable "+") (ExprLiteral (LiteralInt 1)))
          (ExprApplication (ExprApplication (ExprVariable "*") (ExprLiteral (LiteralInt 2))) (ExprLiteral (LiteralInt 3))))

    it "parseProgram" $ do
      testProgramParser parseProgram [r|
id x = x
main = id 2
|]
        (Program [Function () "id"  ["x"] (ExprVariable "x"), Function () "main" [] (ExprApplication (ExprVariable "id") (ExprLiteral (LiteralInt 2)))] [] [] [] [])

    it "parseExprApplication" $ do
      testParser parseExprApplication "f x" (ExprApplication (ExprVariable "f") (ExprVariable "x"))

    it "parses a simple doubling program" $ do
      testProgramParser parseProgram [r|
main = double 21
double x = x + x
|]
        (Program [ Function () "main" [] (ExprApplication (ExprVariable "double") (ExprLiteral (LiteralInt 21)))
        , Function () "double" ["x"] (ExprApplication (ExprApplication (ExprVariable "+") (ExprVariable "x")) (ExprVariable "x"))
        ] [] [] [] [])

    it "num plus string - parses" $ do
      testProgramParser parseProgram "main = 1 + \"hello\"" (Program [Function () "main" [] ((ExprApplication (ExprApplication (ExprVariable "+") (ExprLiteral (LiteralInt 1)))) (ExprLiteral (LiteralString "hello")))] [] [] [] [])

    it "parses a program with many functions" $ do
      testProgramParser parseProgram [r|
id x = x
f p = (id p) * p
double n = n * 2
main = f (double 4)
|]
        (Program [Function () "id" ["x"] (ExprVariable "x"), Function () "f" ["p"] (ExprApplication (ExprApplication (ExprVariable "*") (ExprApplication (ExprVariable "id") (ExprVariable "p"))) (ExprVariable "p")), Function () "double" ["n"] (ExprApplication (ExprApplication (ExprVariable "*") (ExprVariable "n")) (ExprLiteral (LiteralInt 2))), Function () "main" [] (ExprApplication (ExprVariable "f") (ExprApplication (ExprVariable "double") (ExprLiteral (LiteralInt 4))))] [] [] [] [])
