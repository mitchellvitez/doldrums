{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Language
  ( module Language
  )
where

import Data.List (intercalate)
import Data.String (IsString)
import Data.Text (Text, unpack)
import Text.Megaparsec (SourcePos)

data Program a = Program
  { functions :: [Function a]
  , dataDeclarations :: [DataDeclaration]
  }
  deriving Eq
deriving instance Show (Program SourcePos)

instance Show (Program ()) where
  show (Program funcs decls) = "data declarations: " <> show decls <> "\n" <> intercalate "\n" (map show funcs)

instance Semigroup (Program a) where
  Program f1 d1 <> Program f2 d2 = Program (f1 <> f2) (d1 <> d2)

instance Functor Program where
  fmap f (Program funcs datas) = Program (fmap f <$> funcs) datas

data DataDeclaration = DataDeclaration
  -- ... = [Nothing :: Maybe a, Just :: a -> Maybe a]
  { declarations :: [(Tag, [TypeRef])]
  -- data [Maybe] a = ...
  , dataType :: DataType
  -- forall [a, b]. ...
  , typeParameters :: [Name]
  }
  deriving (Eq, Show)

data TypeRef
  = TypeRefVar Name
  | TypeRefConstructor DataType
  | TypeRefApp DataType [TypeRef]
  deriving (Eq, Show)

-- name, list of arguments, body
data Function a = Function
  { annot :: a
  , name :: Name
  , args :: [Name]
  , body :: AnnotatedExpr a
  }
  deriving Eq
deriving instance Show (Function SourcePos)

instance Show (Function ()) where
  show (Function _ name args body) =
    unpack (unName name) <> " " <> intercalate " " (map (unpack . unName) args) <> " = " <> show body

instance Functor Function where
  fmap f (Function annot name args body) = Function (f annot) name args (f <$> body)

-- pattern matching
data Pattern
  = PatternVar Name
  | PatternWildcard
  | PatternLiteral Literal
  | PatternConstructor Tag [Pattern]
  deriving (Eq, Show)

patternNames :: Pattern -> [Name]
patternNames PatternWildcard = []
patternNames (PatternVar n) = [n]
patternNames (PatternLiteral _) = []
patternNames (PatternConstructor _ ps) = concatMap patternNames ps

-- case name, variables to unpack, body
data CaseAlternative a = Alternative Pattern (AnnotatedExpr a)
  deriving Eq
deriving instance Show (CaseAlternative SourcePos)
deriving instance Show (CaseAlternative ())

-- variable names
newtype Name = Name { unName :: Text }
  deriving (Eq, Show, Ord)
  deriving newtype IsString

-- constructor tags like Just, Nothing, True, False
newtype Tag = Tag { unTag :: Text }
  deriving (Eq, Show, Ord)
  deriving newtype IsString

-- types like Maybe, Bool, List
newtype DataType = DataType { unDataType :: Text }
  deriving (Eq, Show)

-- number of arguments to a data constructor
newtype Arity = Arity { unArity :: Int }
  deriving (Eq, Show)
  deriving newtype Num

data Literal
  = LiteralInt Integer
  | LiteralString Text
  | LiteralFloat Double
  deriving (Eq, Show)

data AnnotatedExpr a
  = AnnExprVariable a Name
  | AnnExprLiteral a Literal
  | AnnExprConstructor a Tag Arity
  | AnnExprApplication a (AnnotatedExpr a) (AnnotatedExpr a)
  | AnnExprLet a [(Name, AnnotatedExpr a)] (AnnotatedExpr a)
  | AnnExprLambda a Name (AnnotatedExpr a)
  | AnnExprCase a (AnnotatedExpr a) [CaseAlternative a]
  deriving Eq
deriving instance Show (AnnotatedExpr SourcePos)

annotation :: AnnotatedExpr a -> a
annotation (AnnExprLiteral annot _)           = annot
annotation (AnnExprVariable annot _)      = annot
annotation (AnnExprApplication annot _ _) = annot
annotation (AnnExprConstructor annot _ _) = annot
annotation (AnnExprLet annot _ _)       = annot
annotation (AnnExprLambda annot _ _)      = annot
annotation (AnnExprCase annot _ _)        = annot

instance Functor AnnotatedExpr where
  fmap f (AnnExprLiteral a l) = AnnExprLiteral (f a) l
  fmap f (AnnExprVariable a name) = AnnExprVariable (f a) name
  fmap f (AnnExprApplication a expr1 expr2) = AnnExprApplication (f a) (f <$> expr1) (f <$> expr2)
  fmap f (AnnExprConstructor a tag arity) = AnnExprConstructor (f a) tag arity
  fmap f (AnnExprLet a bindings body) = AnnExprLet (f a) [(name, f <$> binding) | (name, binding) <- bindings] (f <$> body)
  fmap f (AnnExprLambda a name expr) = AnnExprLambda (f a) name (f <$> expr)
  fmap f (AnnExprCase a expr alters) = AnnExprCase (f a) (f <$> expr) (map (\(Alternative pat expr) -> (Alternative pat (f <$> expr))) alters)

-- unparametrized AST, recovered by the below
-- data Expr
--   = ExprVariable Name
--   | ExprLiteral Literal
--   | ExprConstructor Tag Arity
--   | ExprApplication Expr Expr
--   | ExprLet [(Name, Expr)] Expr
--   | ExprLambda Name Expr
--   | ExprCase Expr [CaseAlternative]
--   deriving (Show, Eq, Ord)

type Expr = AnnotatedExpr ()

instance Show Expr where
  show (ExprVariable name) = show name
  show (ExprLiteral l) = show l
  show (ExprConstructor tag arity) = "(Constr " <> show tag <> "," <> show arity <> ")"
  show (ExprApplication a b) = "(App " <> show a <> " " <> show b <> ")"
  show (ExprLet bindings body) = "(Let " <> show bindings <> " " <> show body <> ")"
  show (ExprLambda name expr) = "(Lam " <> show name <> " " <> show expr <> ")"
  show (ExprCase expr alts) = "(Case " <> show expr <> " " <> show alts <> ")"
  show _ = error "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms"

ignoreAnnotation :: AnnotatedExpr a -> Expr
ignoreAnnotation = fmap (const ())

pattern ExprLiteral :: Literal -> Expr
pattern ExprLiteral n <- AnnExprLiteral _ n
  where ExprLiteral n = AnnExprLiteral () n

pattern ExprConstructor :: Tag -> Arity -> Expr
pattern ExprConstructor tag arity <- AnnExprConstructor _ tag arity
  where ExprConstructor tag arity = AnnExprConstructor () tag arity

pattern ExprVariable :: Name -> Expr
pattern ExprVariable name <- AnnExprVariable _ name
  where ExprVariable name = AnnExprVariable () name

pattern ExprApplication :: Expr -> Expr -> Expr
pattern ExprApplication a b <- AnnExprApplication _ a b
  where ExprApplication a b = AnnExprApplication () a b

pattern ExprLambda :: Name -> Expr -> Expr
pattern ExprLambda name expr <- AnnExprLambda _ name expr
  where ExprLambda name expr = AnnExprLambda () name expr

pattern ExprLet :: [(Name, Expr)] -> Expr -> Expr
pattern ExprLet bindings body <- AnnExprLet _ bindings body
  where ExprLet bindings body = AnnExprLet () bindings body

pattern ExprCase :: Expr -> [CaseAlternative ()] -> Expr
pattern ExprCase expr alters <- AnnExprCase _ expr alters
  where ExprCase expr alters = AnnExprCase () expr alters
