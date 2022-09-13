{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec (SourcePos)

-- a Program is a list of top-level definitions
type Program = [TopLevelDefn]

-- name, list of arguments, body
type TopLevelDefn = (Name, [Name], AnnotatedExpr SourcePos)

type Name = Text
type Tag = Int
type Arity = Int

data AnnotatedExpr a
  = AnnExprVariable a Name
  | AnnExprInt a Integer
  | AnnExprBool a Bool
  | AnnExprString a Text
  | AnnExprDouble a Double
  | AnnExprConstructor a Tag Arity
  | AnnExprApplication a (AnnotatedExpr a) (AnnotatedExpr a)
  | AnnExprLet a Name (AnnotatedExpr a) (AnnotatedExpr a)
  | AnnExprLambda a Name (AnnotatedExpr a)
deriving instance Show (AnnotatedExpr SourcePos)

annotation :: AnnotatedExpr a -> a
annotation (AnnExprInt annot _)           = annot
annotation (AnnExprVariable annot _)      = annot
annotation (AnnExprApplication annot _ _) = annot
annotation (AnnExprBool annot _)          = annot
annotation (AnnExprString annot _)        = annot
annotation (AnnExprDouble annot _)        = annot
annotation (AnnExprConstructor annot _ _) = annot
annotation (AnnExprLet annot _ _ _)       = annot
annotation (AnnExprLambda annot _ _)      = annot

instance Functor AnnotatedExpr where
  fmap f (AnnExprInt a x) = AnnExprInt (f a) x
  fmap f (AnnExprVariable a x) = AnnExprVariable (f a) x
  fmap f (AnnExprApplication a b c) = AnnExprApplication (f a) (f <$> b) (f <$> c)
  fmap f (AnnExprBool a x) = AnnExprBool (f a) x
  fmap f (AnnExprString a x) = AnnExprString (f a) x
  fmap f (AnnExprDouble a x) = AnnExprDouble (f a) x
  fmap f (AnnExprConstructor a b c) = AnnExprConstructor (f a) b c
  fmap f (AnnExprLet a b c d) = AnnExprLet (f a) b (f <$> c) (f <$> d)
  fmap f (AnnExprLambda a b c) = AnnExprLambda (f a) b (f <$> c)

-- unparametrized AST, recovered by the below
-- data Expr
--   = ExprVariable Name
--   | ExprInt Integer
--   | ExprBool Bool
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

-- not a "lawful" show, condensed for debugging readability
instance Show Expr where
  show (ExprVariable name) = show name
  show (ExprInt n) = show n
  show (ExprBool b) = show b
  show (ExprString s) = show s
  show (ExprDouble d) = show d
  show (ExprConstructor tag arity) = "Pack{" <> show tag <> "," <> show arity <> "}"
  show (ExprApplication a b) = "(App " <> show a <> " " <> show b <> ")"
  show (ExprLet name a b) = "(Let " <> show name <> " " <> show a <> " " <> show b <> ")"
  show (ExprLambda name expr) = "(Lam " <> show name <> " " <> show expr <> ")"
  show _ = error "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms"

void :: Void
void = error "can't evaluate void"

pattern ExprInt :: Integer -> Expr
pattern ExprInt n <- AnnExprInt _ n
  where ExprInt n = AnnExprInt void n

pattern ExprBool :: Bool -> Expr
pattern ExprBool b <- AnnExprBool _ b
  where ExprBool b = AnnExprBool void b

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
