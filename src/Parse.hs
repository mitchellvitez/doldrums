{-# LANGUAGE RecordWildCards #-}

module Parse where

import Language

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
  L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

spaceConsumer :: Parser ()
spaceConsumer =
  L.space (Control.Monad.void $ oneOf (" \t" :: String)) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

lexemeNewline :: Parser a -> Parser a
lexemeNewline = L.lexeme spaceConsumerNewline

data TopLevel a
  = Decl DataDeclaration
  | Func (Function a)
  | TypeSignature (Name, TypeHint)
  | ClassDecl TypeclassDeclaration
  | InstanceDecl (InstanceDeclaration a)

parseProgram :: Parser (Program SourcePos)
parseProgram = topLevelToProgram <$> some parseTopLevel

-- partition a bunch of top-level declarations into a Program
topLevelToProgram :: [TopLevel SourcePos] -> Program SourcePos
topLevelToProgram topLevelDecls = Program
  { functions = [f | Func f <- topLevelDecls]
  , dataDeclarations = [d | Decl d <- topLevelDecls]
  , typeSignatures = [s | TypeSignature s <- topLevelDecls]
  , typeclassDeclarations = [c | ClassDecl c <- topLevelDecls]
  , instanceDeclarations = [i | InstanceDecl i <- topLevelDecls]
  }

parseTopLevel :: Parser (TopLevel SourcePos)
parseTopLevel =
  L.nonIndented spaceConsumerNewline parseEitherTopLevel <* spaceConsumerNewline

parseEitherTopLevel :: Parser (TopLevel SourcePos)
parseEitherTopLevel =
  try (TypeSignature <$> parseSignature) <|>
  try (Decl <$> parseDataDeclaration) <|>
  try (ClassDecl <$> parseClassDeclaration) <|>
  try (InstanceDecl <$> parseInstanceDeclaration) <|>
      (Func <$> parseFunction)

symbol :: Text -> Parser ()
symbol = void . L.symbol' spaceConsumer

parseClassDeclaration :: Parser TypeclassDeclaration
parseClassDeclaration = do
  lexeme $ string "class"
  superclass <- try parseContext <|> pure []
  name <- Name . unTag <$> parseTag
  param <- parseName
  lexeme $ string "where"
  methods <- try (manyIndented parseSignature) <|> fmap pure parseSignature
  pure $ TypeclassDeclaration name param superclass methods

parseInstanceDeclaration :: Parser (InstanceDeclaration SourcePos)
parseInstanceDeclaration = do
  lexeme $ string "instance"
  context <- try parseContext <|> pure []
  className <- Name . unTag <$> parseTag
  instTypes <- many parseTypeHintSingle
  lexeme $ string "where"
  defs <- try (manyIndented parseMethodDef) <|> fmap pure parseMethodDef
  pure $ InstanceDeclaration context className instTypes defs

parseContext :: Parser [(Name, TypeHint)]
parseContext = do
  constraints <- fmap (:[]) parseSingleConstraint <|> parseMultipleConstraints
  lexeme $ string "=>"
  pure constraints

parseSingleConstraint :: Parser (Name, TypeHint)
parseSingleConstraint = do
  name <- Name . unTag <$> parseTag
  typeVar <- parseName
  pure (name, TypeHintVar typeVar)

parseMultipleConstraints :: Parser [(Name, TypeHint)]
parseMultipleConstraints = do
  lexeme $ char '('
  constraints <- parseSingleConstraint `sepBy` lexeme (char ',')
  lexeme $ char ')'
  pure constraints

parseMethodDef :: Parser (Name, AnnotatedExpr SourcePos)
parseMethodDef = do
  methodName <- parseSignatureName
  args <- many parsePattern
  lexeme $ string "="
  body <- parseAnnotatedExpr
  pure (methodName, foldr (\arg b -> AnnExprLambda (annotation b) (freshPatternVar arg) b) body args)
    where
      freshPatternVar (PatternVar n) = n
      freshPatternVar _ = Name "pat"

parseFunction :: Parser (Function SourcePos)
parseFunction = do
  name <- parseName
  args <- many parsePattern
  body <- try parseGuardedBody <|> parseUnguardedBody
  sourcePos <- getSourcePos
  pure $ Function sourcePos name args body

parseGuardedBody :: Parser (AnnotatedExpr SourcePos)
parseGuardedBody = do
  spaceConsumerNewline
  char '|'
  spaceConsumerNewline
  let guardSep = try $ spaceConsumerNewline *> char '|' <* spaceConsumerNewline
  clauses <- parseGuardClause `sepBy1` guardSep
  pure $ desugarGuards clauses

parseUnguardedBody :: Parser (AnnotatedExpr SourcePos)
parseUnguardedBody = do
  L.symbol' spaceConsumerNewline "="
  parseAnnotatedExpr

parseGuardClause :: Parser (AnnotatedExpr SourcePos, AnnotatedExpr SourcePos)
parseGuardClause = do
  guardExpr <- parseAnnotatedExpr
  L.symbol' spaceConsumerNewline "="
  bodyExpr <- parseAnnotatedExpr
  pure (guardExpr, bodyExpr)

{- -- desugars from
f x
  | guardExpr = e1
  | otherwise = e2
-- to
f x = case guardExpr of
  True -> e1
  False -> case otherwise of  -- otherwise == True
    True -> e2
-}
desugarGuards :: [(AnnotatedExpr a, AnnotatedExpr a)] -> AnnotatedExpr a
desugarGuards [(guard, body)] =
  AnnExprCase (annotation guard) guard
    [Alternative (PatternConstructor (Tag "True") []) body]
desugarGuards ((guard, body) : rest) =
  AnnExprCase (annotation guard) guard
    [ Alternative (PatternConstructor (Tag "True") []) body
    , Alternative (PatternConstructor (Tag "False") []) $ desugarGuards rest
    ]
desugarGuards _ = error "desugarGuards: empty guards"

parseDataDeclaration :: Parser DataDeclaration
parseDataDeclaration = do
  lexeme $ string "data"
  dataTy <- parseTag
  typeVars <- many parseName
  lexeme $ char '='
  constructors <- parseConstructorDeclaration `sepBy1` lexeme (char '|')
  pure $ DataDeclaration constructors (DataType $ unTag dataTy) typeVars

parseConstructorDeclaration :: Parser (Tag, [TypeRef])
parseConstructorDeclaration = do
  tag <- parseTag
  argTypes <- many parseTypeRef
  pure (tag, argTypes)

parseTypeRef :: Parser TypeRef
parseTypeRef = do
  lexeme $ spaceConsumer
  try parseParenApp <|> try parseTypeConstructor <|> parseTypeVar
  where
    parseTypeConstructor = TypeRefConstructor . DataType . unTag <$> parseTag
    parseTypeVar = TypeRefVar <$> parseName
    parseParenApp = do
      char '('
      first <- parseTypeConstructor
      args <- many parseTypeRef
      char ')'
      case first of
        TypeRefConstructor dt -> pure $ TypeRefApp dt args
        _ -> fail "Expected type constructor in parenthesized type"

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
  -- level 9
    [ binaryOp "." InfixR
    ]
  -- level 8
  -- level 7
  , [ binaryOp "*" InfixL
    , binaryOp "/" InfixL
    ]
  -- level 6
  , [ binaryOp "+" InfixL
    , binaryOp "-" InfixL
    ]
  -- level 5
  , [ binaryOp "<>" InfixR
    ]
  -- level 4
  , [ binaryOp ">=" InfixN
    , binaryOp "<=" InfixN
    , binaryOp "==" InfixN
    , binaryOp "/=" InfixN
    , binaryOp ">" InfixN
    , binaryOp "<" InfixN
    ]
  -- level 3
  , [ binaryOp "&&" InfixR
    ]
  -- level 2
  , [ binaryOp "||" InfixR
    ]
  -- level 1 (>>=)
  -- level 0
  , [ binaryOp "$" InfixR
    ]
  ]

binaryOp :: Text -> (Parser (Expr -> Expr -> Expr) -> Operator Parser Expr) -> Operator Parser Expr
binaryOp name fixity =
  fixity $ binaryOpAST name <$ (lexemeNewline . try) (string name <* notFollowedBy operatorChar)

binaryOpAST :: Text -> Expr -> Expr -> Expr
binaryOpAST "$" expr1 expr2 =
  ExprApplication expr1 expr2
binaryOpAST "." expr1 expr2 =
  ExprLambda (Name "compositionresult") (ExprApplication expr1 (ExprApplication expr2 (ExprVariable (Name "compositionresult"))))
binaryOpAST name expr1 expr2 =
  ExprApplication (ExprApplication (ExprVariable (Name name)) expr1) expr2

prefixOp :: Text -> Operator Parser Expr
prefixOp name = Prefix $ prefixOpAST name <$ (lexemeNewline . try) (string name)

prefixOpAST :: Text -> Expr -> Expr
prefixOpAST name expr = ExprApplication (ExprVariable (Name name)) expr

parseLiteral :: Parser Literal
parseLiteral =
  try (LiteralDouble <$> parseDouble) <|>
  LiteralInt <$> parseInt            <|>
  LiteralString <$> parseString

parseExprLiteral :: Parser Expr
parseExprLiteral =
  ExprLiteral <$> parseLiteral

parseExprConstructor :: Parser Expr
parseExprConstructor = do
  tag <- parseTag
  pure $ ExprConstructor tag $ Arity (-1)

-- parse an indented block of multiple items. handles hanging indents
manyIndented :: Parser a -> Parser [a]
manyIndented parseSomething = do
  eol
  spaceConsumerNewline
  ref <- L.indentLevel
  first <- parseSomething
  rest <- many . try $ L.indentGuard spaceConsumerNewline EQ ref *> parseSomething
  pure (first : rest)

parseExprLet :: Parser Expr
parseExprLet = do
  lexeme $ string "let"
  definitions <- try (manyIndented parseDefinition) <|> fmap pure parseDefinition
  spaceConsumerNewline
  lexeme $ string "in"
  spaceConsumerNewline
  body <- parseExpr
  pure $ ExprLet definitions body

parseExprCase :: Parser Expr
parseExprCase = do
  lexeme $ string "case"
  scrutinee <- parseExpr
  lexeme $ string "of"
  alternatives <- try $ manyIndented parseCaseAlternative
  pure $ ExprCase scrutinee alternatives

parsePattern :: Parser Pattern
parsePattern =
  PatternVar <$> parseName <|>
  PatternWildcard <$ lexeme (char '_') <|>
  PatternLiteral <$> parseLiteral <|>
  PatternConstructor <$> parseTag <*> many parsePattern <|>
  between (lexeme $ char '(') (lexeme $ char ')') parsePattern

parseCaseAlternative :: Parser (CaseAlternative ())
parseCaseAlternative = do
  pat <- parsePattern
  lexeme $ string "->"
  expr <- parseExpr
  pure $ Alternative pat expr

parseDefinition :: Parser (Name, Expr)
parseDefinition = do
  name <- parseName
  lexeme $ char '='
  body <- parseExpr
  pure (name, body)

parseExprLambda :: Parser Expr
parseExprLambda = try parseExprLambdaCase <|> parseExprLambdaStandard

parseExprLambdaCase :: Parser Expr
parseExprLambdaCase = do
  lexeme . try $ string "\\case" <* notFollowedBy parseNameChar
  alternatives <- manyIndented parseCaseAlternative
  let var = Name "caseVar"
  pure $ ExprLambda var (ExprCase (ExprVariable var) alternatives)

parseExprLambdaStandard :: Parser Expr
parseExprLambdaStandard = do
  lexeme $ char '\\'
  abstractions <- some parseName
  lexeme $ string "->"
  body <- parseExpr
  pure $ Prelude.foldr ExprLambda body abstractions

parseExprApplication :: Parser Expr
parseExprApplication = do
  (firstExpr:restExprs) <- some parseAtomicExpr
  pure $ Prelude.foldl ExprApplication firstExpr restExprs

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

parseNumber :: Num a => Parser a -> Parser a
parseNumber parseUnsigned = lexeme . try $ do
  sign <- optional . try $ char '-' <* lookAhead digitChar
  n <- parseUnsigned
  pure $ case sign of
    Just _ -> -n
    Nothing -> n

parseInt :: Parser Integer
parseInt = parseNumber L.decimal

parseDouble :: Parser Double
parseDouble = parseNumber L.float

parseString :: Parser Text
parseString = lexeme $ char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

parseTag :: Parser Tag
parseTag = lexeme $ try $ do
  first <- upperChar
  rest <- many parseNameChar
  pure . Tag $ T.pack (first : rest)
  -- can't be a keyword due to first upperChar

parseName :: Parser Name
parseName = lexeme . try $ do
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

parseSignature :: Parser (Name, TypeHint)
parseSignature = do
  name <- parseSignatureName
  lexeme $ string "::"
  signature <- try parseConstrainedTypeHint <|> parseTypeHint
  pure (name, signature)

parseConstrainedTypeHint :: Parser TypeHint
parseConstrainedTypeHint = do
  constraints <- try (fmap (:[]) parseSingleConstraint) <|> parseMultipleConstraints
  lexeme $ string "=>"
  body <- parseTypeHint
  pure $ TypeHintConstraint constraints body

parseSingleConstraintInline :: Parser (Name, TypeHint)
parseSingleConstraintInline = do
  name <- Name . unTag <$> parseTag
  arg <- parseTypeHintSingle
  pure (name, arg)

parseSignatureName :: Parser Name
parseSignatureName =
  try parseOperatorInParens <|> parseName

parseOperatorInParens :: Parser Name
parseOperatorInParens = do
  lexeme $ char '('
  name <- Name . T.pack <$> some operatorChar
  lexeme $ char ')'
  pure name

operatorChar :: Parser Char
operatorChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)

parseTypeHint :: Parser TypeHint
parseTypeHint= do
  types <- parseTypeHintSingle `sepBy1` try (lexeme $ string "->")
  pure $ foldr1 (:~>) types

parseTypeHintSingle :: Parser TypeHint
parseTypeHintSingle = do
  first <- parseTypeHintAtomic
  rest <- many parseTypeHintAtomic
  case (first, rest) of
    (TypeHintConstructor dt, args) -> pure $ TypeHintApp dt args
    (TypeHintVar _, []) -> pure first
    (TypeHintVar _, _:_) -> fail "Type variable applied to arguments"
    _ -> pure first

parseTypeHintAtomic :: Parser TypeHint
parseTypeHintAtomic =
  typeHintLiteral "Int" TypeHintInt <|>
  typeHintLiteral "Double" TypeHintDouble <|>
  typeHintLiteral "String" TypeHintString <|>
  try (TypeHintVar <$> parseName) <|>
  try (TypeHintConstructor . DataType . unTag <$> parseTag) <|>
  between (lexeme $ char '(') (lexeme $ char ')') parseTypeHint

typeHintLiteral :: Text -> TypeHint -> Parser TypeHint
typeHintLiteral name hint = do
  try . lexeme $ string name <* notFollowedBy parseNameChar
  pure hint

keywords :: Set Text
keywords = Set.fromList
  ["let", "in", "case", "of", "data", "class", "instance", "where"]
