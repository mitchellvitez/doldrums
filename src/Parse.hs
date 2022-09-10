module Parse where

import Language

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data PartialExpr = NoOp | FoundOp Name Expr

spaceConsumer =
  L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- TODO: replace Void with a better error handling mechanism
type Parser = Parsec Void Text

parseProgram :: Parser Program
parseProgram = do
  spaceConsumer
  some parseSupercombinator

parseSupercombinator :: Parser SupercombinatorDefinition
parseSupercombinator = do
  name <- parseName
  vars <- many parseName
  lexeme $ char '='
  body <- parseAnnotatedExpr
  lexeme $ char ';'
  pure $ (name, vars, body)

parseAnnotatedExpr :: Parser (Annotated Expr)
parseAnnotatedExpr = do
  expr <- parseExpr
  sourcePos <- getSourcePos
  pure $ Annotated expr $ Annotation sourcePos

parseExpr :: Parser Expr
parseExpr =
  try parseExprLet    <|>
  try parseExprLambda <|>
  parseExpr0

assembleOp :: Expr -> PartialExpr -> Expr
assembleOp expr NoOp = expr
assembleOp expr1 (FoundOp op expr2) =
  ExprApplication (ExprApplication (ExprVariable op) expr1) expr2

parseEmpty :: Parser PartialExpr
parseEmpty = do
  string ""
  pure NoOp

parseExpr0 :: Parser Expr
parseExpr0 = assembleOp <$> parseExpr1 <*> parseExpr0Partial

parseExpr0Partial :: Parser PartialExpr
parseExpr0Partial =
      parsePartialOp "$" parseExpr0
  <|> parseEmpty

parseExpr1 :: Parser Expr
parseExpr1 = assembleOp <$> parseExpr2 <*> parseExpr1Partial

parseExpr1Partial :: Parser PartialExpr
parseExpr1Partial =
      parsePartialOp "||" parseExpr1
  <|> parseEmpty

parseExpr2 :: Parser Expr
parseExpr2 = assembleOp <$> parseExpr3 <*> parseExpr2Partial

parseExpr2Partial :: Parser PartialExpr
parseExpr2Partial =
      parsePartialOp "&&" parseExpr2
  <|> parseEmpty

parseExpr3 :: Parser Expr
parseExpr3 = assembleOp <$> parseExpr4 <*> parseExpr3Partial

parseExpr3Partial :: Parser PartialExpr
parseExpr3Partial =
      parsePartialOp ">=" parseExpr3
  <|> parsePartialOp "<=" parseExpr3
  <|> parsePartialOp "==" parseExpr3
  <|> parsePartialOp "!=" parseExpr3
  <|> parsePartialOp ">"  parseExpr3
  <|> parsePartialOp "<"  parseExpr3
  <|> parseEmpty

parseExpr4 :: Parser Expr
parseExpr4 = assembleOp <$> parseExpr5 <*> parseExpr4Partial

parseExpr4Partial :: Parser PartialExpr
parseExpr4Partial =
      try (parsePartialOp "+." parseExpr4)
  <|> try (parsePartialOp "-." parseExpr4)
  <|> parsePartialOp "+" parseExpr4
  <|> parsePartialOp "-" parseExpr4
  <|> parseEmpty

parseExpr5 :: Parser Expr
parseExpr5 = assembleOp <$> parseExpr6 <*> parseExpr5Partial

parseExpr5Partial :: Parser PartialExpr
parseExpr5Partial =
      try (parsePartialOp "*." parseExpr5)
  <|> try (parsePartialOp "/." parseExpr5)
  <|> parsePartialOp "*" parseExpr5
  <|> parsePartialOp "/" parseExpr5
  <|> parseEmpty

parseExpr6 :: Parser Expr
parseExpr6 =
  parseNot <|>
  parseNegate <|>
  parseExprApplication

parseNot = do
  char '!'
  expr <- parseExpr6
  pure $ ExprApplication (ExprVariable "!") expr

parseNegate = do
  char '~'
  expr <- parseExpr6
  pure $ ExprApplication (ExprVariable "~") expr

parsePartialOp :: Name -> Parser Expr -> Parser PartialExpr
parsePartialOp opcode parserLevel = do
  op <- lexeme $ string opcode
  rest <- parserLevel
  pure $ FoundOp opcode rest

parseAtomicExpr :: Parser Expr
parseAtomicExpr =
  parseExprVariable    <|>
  parseExprLiteral     <|>
  parseExprConstructor <|>
  parseExprParenthesized

parseExprParenthesized :: Parser Expr
parseExprParenthesized = do
  lexeme $ char '('
  body <- parseExpr
  lexeme $ char ')'
  pure body

parseExprLiteral :: Parser Expr
parseExprLiteral =
  ExprLiteral . ValueInt  . toInteger <$> parseInt <|>
  ExprLiteral . ValueDouble <$> parseDouble <|>
  ExprLiteral . ValueBool   <$> parseBool   <|>
  ExprLiteral . ValueString <$> parseString

parseExprVariable :: Parser Expr
parseExprVariable = ExprVariable <$> parseName

parseExprConstructor :: Parser Expr
parseExprConstructor = do
  lexeme $ string "Pack"
  lexeme $ char '{'
  tag <- parseInt
  lexeme $ char ','
  arity <- parseInt
  lexeme $ char '}'
  pure $ ExprConstructor tag arity

parseExprLet :: Parser Expr
parseExprLet = do
  lexeme $ string "let"
  definitions <- parseDefinition `sepBy1` lexeme (char ',')
  lexeme $ string "in"
  body <- parseExpr
  pure $ ExprLet definitions body

parseDefinition :: Parser (Name, Expr)
parseDefinition = do
  name <- parseName
  lexeme $ char '='
  body <- parseExpr
  pure (name, body)

parseExprLambda :: Parser Expr
parseExprLambda = do
  lexeme $ char '\\'
  abstractions <- some parseName
  lexeme $ char '.'
  body <- parseExpr
  pure $ ExprLambda abstractions body

parseExprApplication :: Parser Expr
parseExprApplication = do
  exprs <- some parseAtomicExpr
  pure $ makeApplicationChain exprs

makeApplicationChain :: [Expr] -> Expr
makeApplicationChain exprs = Prelude.foldl ExprApplication (Prelude.head exprs) (Prelude.tail exprs)

parseInt :: Parser Int
parseInt = lexeme L.decimal

parseDouble :: Parser Double
parseDouble = L.float

parseBool :: Parser Bool
parseBool = do
  s <- string "True" <|> string "False"
  pure $ case s of
    "True" -> True
    "False" -> False

parseString :: Parser Text
parseString = char '"' >> pack <$> manyTill L.charLiteral (char '"')

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
keywords = ["let", "in", "of", "Pack", "True", "False"]
