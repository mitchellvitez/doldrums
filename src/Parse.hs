module Parse where

import Language

import Control.Monad.Combinators.Expr
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as Set
import Data.Set (Set)
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
  some parseTopLevelDefn

parseTopLevelDefn :: Parser TopLevelDefn
parseTopLevelDefn = do
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
  pure $ Annotated sourcePos expr

parseExpr :: Parser Expr
parseExpr =
  try parseExprLet    <|>
  try parseExprLambda <|>
  parseOperator

parseOperator :: Parser Expr
parseOperator = makeExprParser parseExprApplication opTable <?> "expression"

-- Table of all operators, ordered by precedence level
-- If you add a new operator here, add a new type to primitiveTypes
opTable :: [[Operator Parser Expr]]
opTable =
  [
  -- level 6
    [ prefixOp "~"
    , prefixOp "!"
    ]
  -- level 5
  , [ binaryOp "*."
    , binaryOp "/."
    , binaryOp "*"
    , binaryOp "/"
    ]
  -- level 4
  , [ binaryOp "+."
    , binaryOp "-."
    , binaryOp "+"
    , binaryOp "-"
    ]
  -- level 3
  , [ binaryOp ">="
    , binaryOp "<="
    , binaryOp "=="
    , binaryOp "!="
    , binaryOp ">"
    , binaryOp "<"
    ]
  -- level 2
  , [ binaryOp "&&"
    ]
  -- level 1
  , [ binaryOp "||"
    ]
  -- level 0
  , [ binaryOp "$"
    ]
  ]

binaryOp :: Text -> Operator Parser Expr
binaryOp name = InfixR $ binaryOpAST name <$ (lexeme . try) (string name)

binaryOpAST :: Text -> Expr -> Expr -> Expr
binaryOpAST "$" expr1 expr2 =
  ExprApplication expr1 expr2
binaryOpAST name expr1 expr2 =
  ExprApplication (ExprApplication (ExprVariable name) expr1) expr2

prefixOp :: Text -> Operator Parser Expr
prefixOp name = Prefix $ prefixOpAST name <$ (lexeme . try) (string name)

prefixOpAST :: Text -> Expr -> Expr
prefixOpAST name expr = ExprApplication (ExprVariable name) expr

parseExprLiteral :: Parser Expr
parseExprLiteral =
  try (ExprDouble <$> parseDouble) <|>
  ExprInt <$> parseInt   <|>
  ExprBool  <$> parseBool    <|>
  ExprString <$> parseString

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
  pure $ Prelude.foldr (\(name, expr) -> ExprLet name expr) body definitions

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
  pure $ Prelude.foldr ExprLambda body abstractions

parseExprApplication :: Parser Expr
parseExprApplication = do
  exprs <- some parseAtomicExpr
  pure $ Prelude.foldl ExprApplication (Prelude.head exprs) (Prelude.tail exprs)

parseAtomicExpr :: Parser Expr
parseAtomicExpr =
  parseExprVariable    <|>
  parseExprLiteral     <|>
  parseExprConstructor <|>
  parseExprParenthesized

parseExprVariable :: Parser Expr
parseExprVariable = ExprVariable <$> parseName

parseExprParenthesized :: Parser Expr
parseExprParenthesized = do
  lexeme $ char '('
  body <- parseExpr
  lexeme $ char ')'
  pure body

parseInt :: Integral i => Parser i
parseInt = lexeme L.decimal

parseDouble :: Parser Double
parseDouble = lexeme L.float

parseBool :: Parser Bool
parseBool = do
  s <- string "True" <|> string "False"
  case s of
    "True" -> pure True
    "False" -> pure False
    _ -> fail "Couldn't parse bool"

parseString :: Parser Text
parseString = char '"' >> pack <$> manyTill L.charLiteral (char '"')

parseName :: Parser Name
parseName = lexeme $ try $ do
  first <- letterChar
  rest <- many parseNameChar
  let name = pack (first : rest)
  if name `Set.member` keywords
    then fail "found keyword when looking for variable"
    else pure name

parseNameChar :: Parser Char
parseNameChar =
  letterChar <|>
  digitChar  <|>
  char '_'

keywords :: Set Text
keywords = Set.fromList ["let", "in", "of", "Pack", "True", "False"]
