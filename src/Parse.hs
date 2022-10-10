{-# LANGUAGE RecordWildCards #-}

module Parse where

import Language

import Data.List as List (foldr)
import Control.Monad.Combinators.Expr
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumerNewline :: Parser ()
spaceConsumerNewline =
  L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

spaceConsumer :: Parser ()
spaceConsumer =
  L.space (Control.Monad.void $ oneOf (" \t" :: String)) (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

data TopLevel a = Decl DataDeclaration | Func (Function a)

parseProgram :: Parser (Program SourcePos)
parseProgram = do
  topLevels <- some parseTopLevel
  pure $ topLevelToProgram (Program [] []) topLevels

topLevelToProgram :: Program SourcePos -> [TopLevel SourcePos] -> Program SourcePos
topLevelToProgram Program{..} [] = Program (reverse functions) (reverse dataDeclarations)
topLevelToProgram Program{..} (topLevel:rest) = case topLevel of
  Decl (d@(DataDeclaration _ _)) -> topLevelToProgram Program{..} { dataDeclarations = d : dataDeclarations } rest
  Func (f@(Function _ _ _ _)) -> topLevelToProgram Program{..} { functions = f : functions } rest

parseTopLevel :: Parser (TopLevel SourcePos)
parseTopLevel =
  L.nonIndented spaceConsumerNewline parseEitherTopLevel <* spaceConsumerNewline

parseEitherTopLevel :: Parser (TopLevel SourcePos)
parseEitherTopLevel =
  try (Decl <$> parseDataDeclaration) <|>
      (Func <$> parseFunction)

symbol :: Text -> Parser ()
symbol s = L.symbol' spaceConsumer s >> return ()

parseFunction :: Parser (Function SourcePos)
parseFunction = do
  name <- parseName
  args <- many parseName
  symbol "="
  body <- parseAnnotatedExpr
  sourcePos <- getSourcePos
  pure $ Function sourcePos name args body

parseDataDeclaration :: Parser DataDeclaration
parseDataDeclaration = do
  lexeme $ string "data"
  dataTy <- parseTag
  lexeme $ char '='
  constructors <- parseConstructorDeclaration `sepBy1` lexeme (char '|')
  pure $ DataDeclaration constructors (DataType $ unTag dataTy)

parseConstructorDeclaration :: Parser (Tag, Arity)
parseConstructorDeclaration = do
  tag <- parseTag
  arity <- parseInt
  pure (tag, Arity arity)

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
  ExprApplication (ExprApplication (ExprVariable (Name name)) expr1) expr2

prefixOp :: Text -> Operator Parser Expr
prefixOp name = Prefix $ prefixOpAST name <$ (lexeme . try) (string name)

prefixOpAST :: Text -> Expr -> Expr
prefixOpAST name expr = ExprApplication (ExprVariable (Name name)) expr

parseExprLiteral :: Parser Expr
parseExprLiteral =
  try (ExprDouble <$> parseDouble) <|>
  ExprInt <$> parseInt             <|>
  ExprString <$> parseString       <|>
  parseExprConstructor

parseExprConstructor :: Parser Expr
parseExprConstructor = do
  tag <- parseTag
  pure $ ExprConstructor tag $ Arity (-1)

parseExprLet :: Parser Expr
parseExprLet = do
  definitions <- L.indentBlock spaceConsumerNewline $ do
    lexeme $ string "let"
    pure $ L.IndentSome Nothing pure parseDefinition
  lexeme $ string "in"
  body <- parseExpr
  pure $ List.foldr (\(name, binding) -> ExprLet name binding) body definitions

parseExprCase :: Parser Expr
parseExprCase =
  L.indentBlock spaceConsumerNewline $ do
    lexeme $ string "case"
    scrutinee <- parseExpr
    lexeme $ string "of"
    pure $ L.IndentSome Nothing (pure . ExprCase scrutinee) parseCaseAlternative

parseCaseAlternative :: Parser (CaseAlternative ())
parseCaseAlternative = do
  caseName <- parseTag
  names <- many parseName
  lexeme $ string "->"
  expr <- parseExpr
  pure $ Alternative caseName names expr

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
  lexeme $ string "->"
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
parseString = lexeme $ char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

parseTag :: Parser Tag
parseTag = lexeme $ try $ do
  first <- upperChar
  rest <- many parseNameChar
  pure . Tag $ T.pack (first : rest)
  -- can't be a keyword due to first upperChar

parseName :: Parser Name
parseName = lexeme $ try $ do
  first <- lowerChar
  rest <- many parseNameChar
  let name = T.pack (first : rest)
  if name `Set.member` keywords
    then fail "found keyword when looking for variable"
    else pure $ Name name

parseNameChar :: Parser Char
parseNameChar =
  letterChar <|>
  digitChar  <|>
  char '_'   <|>
  char '\''

keywords :: Set Text
keywords = Set.fromList ["let", "in", "case", "of", "data"]
