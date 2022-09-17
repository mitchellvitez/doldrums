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

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

parseProgram :: Parser (Program SourcePos)
parseProgram = do
  spaceConsumer
  topLevels <- parseTopLevel `endBy1` lexeme (char ';')
  pure $ reversals $ accumTopLevels topLevels $ Program [] []
  where
    accumTopLevels [] x = x
    accumTopLevels (Func function : rest) program = accumTopLevels rest
      program { functions = function : functions program }
    accumTopLevels (Decl dataDeclaration : rest) program = accumTopLevels rest
      program { dataDeclarations = dataDeclaration : dataDeclarations program }
    reversals (Program f d) = Program (Prelude.reverse f) (Prelude.reverse d)

data TopLevel a = Decl DataDeclaration | Func (Function a)

parseTopLevel :: Parser (TopLevel SourcePos)
parseTopLevel =
  try (Decl <$> parseDataDeclaration) <|>
  Func <$> parseFunction

parseFunction :: Parser (Function SourcePos)
parseFunction = do
  name <- parseName
  args <- many parseName
  lexeme $ char '='
  body <- parseAnnotatedExpr
  pure $ Function name args body

parseDataDeclaration :: Parser DataDeclaration
parseDataDeclaration = do
  lexeme $ string "data"
  constructors <- parseConstructorDeclaration `sepBy1` lexeme (char '|')
  pure $ DataDeclaration constructors

parseConstructorDeclaration :: Parser (Tag, Arity)
parseConstructorDeclaration = do
  tag <- parseCaseName
  arity <- parseInt
  pure (tag, arity)

parseAnnotatedExpr :: Parser (AnnotatedExpr SourcePos)
parseAnnotatedExpr = do
  sourcePos <- getSourcePos
  expr <- parseExpr
  pure $ const sourcePos <$> expr

parseExpr :: Parser Expr
parseExpr =
  try parseExprLet    <|>
  try parseExprLambda <|>
  try parseExprCase   <|>
  parseOperator

parseOperator :: Parser Expr
parseOperator = makeExprParser parseExprApplication opTable

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
  ExprInt <$> parseInt             <|>
  ExprString <$> parseString       <|>
  parseExprConstructor

parseExprConstructor :: Parser Expr
parseExprConstructor = do
  tag <- parseCaseName
  pure $ ExprConstructor tag (-1);

parseExprLet :: Parser Expr
parseExprLet = do
  lexeme $ string "let"
  definitions <- parseDefinition `sepBy1` lexeme (char ',')
  lexeme $ string "in"
  body <- parseExpr
  pure $ Prelude.foldr (\(name, expr) -> ExprLet name expr) body definitions

parseExprCase :: Parser Expr
parseExprCase = do
  lexeme $ string "case"
  scrutinee <- parseExpr
  lexeme $ string "of"
  alternatives <- parseCaseAlternative `sepBy1` lexeme (char ',')
  pure $ ExprCase scrutinee alternatives

parseCaseAlternative :: Parser (Tag, [Name], Expr)
parseCaseAlternative = do
  caseName <- parseCaseName
  names <- many parseName
  lexeme $ string "->"
  expr <- parseExpr
  pure (caseName, names, expr)

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

parseString :: Parser Text
parseString = lexeme $ char '"' >> pack <$> manyTill L.charLiteral (char '"')

parseCaseName :: Parser Tag
parseCaseName = lexeme $ try $ do
  first <- upperChar
  rest <- many parseNameChar
  pure $ pack (first : rest)
  -- can't be a keyword due to first upperChar

parseName :: Parser Name
parseName = lexeme $ try $ do
  first <- lowerChar
  rest <- many parseNameChar
  let name = pack (first : rest)
  if name `Set.member` keywords
    then fail "found keyword when looking for variable"
    else pure name

parseNameChar :: Parser Char
parseNameChar =
  letterChar <|>
  digitChar  <|>
  char '_'   <|>
  char '\''

keywords :: Set Text
keywords = Set.fromList ["let", "in", "case", "of", "data"]
