{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Language
  ( module Language
  )
where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec (SourcePos)

data Program a = Program
  { functions :: [Function a]
  , dataDeclarations :: [DataDeclaration]
  }
deriving instance Show (Program SourcePos)
deriving instance Show (Program Void)
deriving instance Eq (Program Void)

instance Semigroup (Program a) where
  Program f1 d1 <> Program f2 d2 = Program (f1 <> f2) (d1 <> d2)

-- tag, arity
-- Just, 1
-- TODO: check that there aren't two constructors in the same program with the same Tag (Tags should be unique)
newtype DataDeclaration = DataDeclaration { unDataDeclaration :: [(Tag, Arity)] }
  deriving (Eq, Show)

-- name, list of arguments, body
data Function a = Function
  { name :: Name
  , args :: [Name]
  , body :: AnnotatedExpr a
  }
deriving instance Show (Function SourcePos)
deriving instance Show (Function Void)
deriving instance Eq (Function Void)

-- case name, variables to unpack, body
type CaseAlternative a = (Tag, [Name], AnnotatedExpr a)

type Name = Text
type Tag = Text
type Arity = Int

data AnnotatedExpr a
  = AnnExprVariable a Name
  | AnnExprInt a Integer
  | AnnExprString a Text
  | AnnExprDouble a Double
  | AnnExprConstructor a Tag Arity
  | AnnExprApplication a (AnnotatedExpr a) (AnnotatedExpr a)
  | AnnExprLet a Name (AnnotatedExpr a) (AnnotatedExpr a)
  | AnnExprLambda a Name (AnnotatedExpr a)
  | AnnExprCase a (AnnotatedExpr a) [CaseAlternative a]
deriving instance Show (AnnotatedExpr SourcePos)

annotation :: AnnotatedExpr a -> a
annotation (AnnExprInt annot _)           = annot
annotation (AnnExprVariable annot _)      = annot
annotation (AnnExprApplication annot _ _) = annot
annotation (AnnExprString annot _)        = annot
annotation (AnnExprDouble annot _)        = annot
annotation (AnnExprConstructor annot _ _) = annot
annotation (AnnExprLet annot _ _ _)       = annot
annotation (AnnExprLambda annot _ _)      = annot
annotation (AnnExprCase annot _ _)        = annot

-- TODO: make this a Bifunctor instead
instance Functor AnnotatedExpr where
  fmap f (AnnExprInt a n) = AnnExprInt (f a) n
  fmap f (AnnExprVariable a name) = AnnExprVariable (f a) name
  fmap f (AnnExprApplication a expr1 expr2) = AnnExprApplication (f a) (f <$> expr1) (f <$> expr2)
  fmap f (AnnExprString a s) = AnnExprString (f a) s
  fmap f (AnnExprDouble a d) = AnnExprDouble (f a) d
  fmap f (AnnExprConstructor a tag arity) = AnnExprConstructor (f a) tag arity
  fmap f (AnnExprLet a name binding body) = AnnExprLet (f a) name (f <$> binding) (f <$> body)
  fmap f (AnnExprLambda a name expr) = AnnExprLambda (f a) name (f <$> expr)
  fmap f (AnnExprCase a expr alters) = AnnExprCase (f a) (f <$> expr) (map (\(n, name, expr) -> (n, name, f <$> expr)) alters)

-- unparametrized AST, recovered by the below
-- data Expr
--   = ExprVariable Name
--   | ExprInt Integer
--   | ExprString Text
--   | ExprDouble Double
--   | ExprConstructor Tag Arity
--   | ExprApplication Expr Expr
--   | ExprLet Name Expr Expr
--   | ExprLambda Name Expr
--   deriving (Show, Eq, Ord, Generic, NFData)

type Expr = AnnotatedExpr Void
deriving instance Eq Expr
deriving instance Ord Expr

annotatedToExpr :: AnnotatedExpr a -> Expr
annotatedToExpr = fmap (const void)

-- not a "lawful" show, condensed for debugging readability
instance Show Expr where
  show (ExprVariable name) = show name
  show (ExprInt n) = show n
  show (ExprString s) = show s
  show (ExprDouble d) = show d
  show (ExprConstructor tag arity) = "(Constr " <> show tag <> "," <> show arity <> ")"
  show (ExprApplication a b) = "(App " <> show a <> " " <> show b <> ")"
  show (ExprLet name a b) = "(Let " <> show name <> " " <> show a <> " " <> show b <> ")"
  show (ExprLambda name expr) = "(Lam " <> show name <> " " <> show expr <> ")"
  show (ExprCase expr alts) = "(Case " <> show expr <> " " <> show alts <> ")"
  show _ = error "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms"

void :: Void
void = error "can't evaluate void"

pattern ExprInt :: Integer -> Expr
pattern ExprInt n <- AnnExprInt _ n
  where ExprInt n = AnnExprInt void n

pattern ExprString :: Text -> Expr
pattern ExprString s <- AnnExprString _ s
  where ExprString s = AnnExprString void s

pattern ExprDouble :: Double -> Expr
pattern ExprDouble d <- AnnExprDouble _ d
  where ExprDouble d = AnnExprDouble void d

pattern ExprConstructor :: Tag -> Arity -> Expr
pattern ExprConstructor tag arity <- AnnExprConstructor _ tag arity
  where ExprConstructor tag arity = AnnExprConstructor void tag arity

pattern ExprVariable :: Name -> Expr
pattern ExprVariable name <- AnnExprVariable _ name
  where ExprVariable name = AnnExprVariable void name

pattern ExprApplication :: Expr -> Expr -> Expr
pattern ExprApplication a b <- AnnExprApplication _ a b
  where ExprApplication a b = AnnExprApplication void a b

pattern ExprLambda :: Name -> Expr -> Expr
pattern ExprLambda name expr <- AnnExprLambda _ name expr
  where ExprLambda name expr = AnnExprLambda void name expr

pattern ExprLet :: Name -> Expr -> Expr -> Expr
pattern ExprLet name binding body <- AnnExprLet _ name binding body
  where ExprLet name binding body = AnnExprLet void name binding body

pattern ExprCase :: Expr -> [CaseAlternative Void] -> Expr
pattern ExprCase expr alters <- AnnExprCase _ expr alters
  where ExprCase expr alters = AnnExprCase void expr alters
