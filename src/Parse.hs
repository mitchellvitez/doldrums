module Parse where

import Language

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data PartialExpr = NoOp | FoundOp Text CoreExpr

spaceConsumer =
  L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- TODO: replace Void with a better error handling mechanism
type Parser = Parsec Void Text

parseProgram :: Parser CoreProgram
parseProgram = do
  spaceConsumer
  some parseSupercombinator

parseSupercombinator :: Parser CoreSupercombinatorDefinition
parseSupercombinator = do
  name <- parseName
  vars <- many parseName
  lexeme $ char '='
  body <- parseExpr
  lexeme $ char ';'
  pure $ (name, vars, body)

parseExpr :: Parser CoreExpr
parseExpr =
  try parseExprLet    <|>
  try parseExprCase   <|>
  try parseExprLambda <|>
  parseExpr0

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp expr NoOp = expr
assembleOp expr1 (FoundOp op expr2) =
  ExprApplication (ExprApplication (ExprVariable op) expr1) expr2

parseEmpty :: Parser PartialExpr
parseEmpty = do
  string ""
  pure NoOp

parseExpr0 :: Parser CoreExpr
parseExpr0 = assembleOp <$> parseExpr1 <*> parseExpr0Partial

parseExpr0Partial :: Parser PartialExpr
parseExpr0Partial =
      parsePartialOp "$" parseExpr1
  <|> parseEmpty

parseExpr1 :: Parser CoreExpr
parseExpr1 = assembleOp <$> parseExpr2 <*> parseExpr1Partial

parseExpr1Partial :: Parser PartialExpr
parseExpr1Partial =
      parsePartialOp "||" parseExpr2
  <|> parseEmpty

parseExpr2 :: Parser CoreExpr
parseExpr2 = assembleOp <$> parseExpr3 <*> parseExpr2Partial

parseExpr2Partial :: Parser PartialExpr
parseExpr2Partial =
      parsePartialOp "&&" parseExpr3
  <|> parseEmpty

parseExpr3 :: Parser CoreExpr
parseExpr3 = assembleOp <$> parseExpr4 <*> parseExpr3Partial

parseExpr3Partial :: Parser PartialExpr
parseExpr3Partial =
      parsePartialOp ">=" parseExpr4
  <|> parsePartialOp "<=" parseExpr4
  <|> parsePartialOp "==" parseExpr4
  <|> parsePartialOp "!=" parseExpr4
  <|> parsePartialOp ">"  parseExpr4
  <|> parsePartialOp "<"  parseExpr4
  <|> parseEmpty

parseExpr4 :: Parser CoreExpr
parseExpr4 = assembleOp <$> parseExpr5 <*> parseExpr4Partial

parseExpr4Partial :: Parser PartialExpr
parseExpr4Partial =
      parsePartialOp "+" parseExpr4
  <|> parsePartialOp "-" parseExpr5
  <|> parseEmpty

parseExpr5 :: Parser CoreExpr
parseExpr5 = assembleOp <$> parseExpr6 <*> parseExpr5Partial

parseExpr5Partial :: Parser PartialExpr
parseExpr5Partial =
      parsePartialOp "*" parseExpr5
  <|> parsePartialOp "/" parseExpr6
  <|> parseEmpty

parseExpr6 :: Parser CoreExpr
parseExpr6 = parseExprApplication

parsePartialOp :: Text -> Parser CoreExpr -> Parser PartialExpr
parsePartialOp opcode parserLevel = do
  op <- lexeme $ string opcode
  rest <- parserLevel
  pure $ FoundOp opcode rest

parseAtomicExpr :: Parser CoreExpr
parseAtomicExpr =
  parseExprVariable    <|>
  parseExprNumber      <|>
  parseExprConstructor <|>
  parseExprParenthesized

parseExprParenthesized :: Parser CoreExpr
parseExprParenthesized = do
  lexeme $ char '('
  body <- parseExpr
  lexeme $ char ')'
  pure body

parseExprNumber :: Parser CoreExpr
parseExprNumber = ExprNumber <$> parseInt

parseExprVariable :: Parser CoreExpr
parseExprVariable = ExprVariable <$> parseName

parseExprConstructor :: Parser CoreExpr
parseExprConstructor = do
  lexeme $ string "Pack"
  lexeme $ char '{'
  tag <- parseInt
  lexeme $ char ','
  arity <- parseInt
  lexeme $ char '}'
  pure $ ExprConstructor tag arity

parseExprLet :: Parser CoreExpr
parseExprLet = do
  lexeme $ string "let"
  definitions <- parseDefinition `sepBy1` lexeme (char ',')
  lexeme $ string "in"
  body <- parseExpr
  pure $ ExprLet definitions body

parseDefinition :: Parser (Name, CoreExpr)
parseDefinition = do
  name <- parseName
  lexeme $ char '='
  body <- parseExpr
  pure (name, body)

parseExprCase :: Parser CoreExpr
parseExprCase = do
  lexeme $ string "case"
  scrutinee <- parseExpr
  lexeme $ string "of"
  alternatives <- parseAlternative `sepBy1` lexeme (char ',')
  pure $ ExprCase scrutinee alternatives

parseAlternative :: Parser CoreAlternative
parseAlternative = do
  lexeme $ char '<'
  tag <- parseInt
  lexeme $ char '>'
  boundVars <- many parseName
  lexeme $ string "->"
  result <- parseExpr
  pure (tag, boundVars, result)

parseExprLambda :: Parser CoreExpr
parseExprLambda = do
  lexeme $ char '\\'
  abstractions <- some parseName
  lexeme $ char '.'
  body <- parseExpr
  pure $ ExprLambda abstractions body

parseExprApplication :: Parser CoreExpr
parseExprApplication = do
  exprs <- some parseAtomicExpr
  pure $ makeApplicationChain exprs

makeApplicationChain :: [CoreExpr] -> CoreExpr
makeApplicationChain exprs = Prelude.foldl ExprApplication (Prelude.head exprs) (Prelude.tail exprs)

parseInt :: Parser Int
parseInt = lexeme L.decimal

parseName :: Parser Name
parseName = lexeme $ try $ do
  first <- letterChar
  rest <- many parseNameChar
  let name = pack (first : rest)
  if name `elem` keywords
    then fail "found keyword when looking for variable"
    else pure name

parseNameChar :: Parser Char
parseNameChar =
  letterChar <|>
  digitChar  <|>
  char '_'

keywords :: [Text]
keywords = ["let", "case", "in", "of", "Pack"]
