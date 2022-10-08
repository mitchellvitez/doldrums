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

import Data.List (intercalate)
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec (SourcePos)

data Program a = Program
  { functions :: [Function a]
  , dataDeclarations :: [DataDeclaration]
  }
  deriving Eq
deriving instance Show (Program Void)
deriving instance Show (Program SourcePos)

instance Show (Program ()) where
  show (Program funcs decls) = "data declarations: " <> show decls <> "\n" <> intercalate "\n" (map show funcs)

instance Semigroup (Program a) where
  Program f1 d1 <> Program f2 d2 = Program (f1 <> f2) (d1 <> d2)

instance Functor Program where
  fmap f (Program funcs datas) = Program (fmap f <$> funcs) datas

data DataDeclaration = DataDeclaration
  { unDataDeclaration :: [(Tag, Arity)]
  , dataType :: Tag
  }
  deriving (Eq, Show)

-- name, list of arguments, body
data Function a = Function
  { annot :: a
  , name :: Name
  , args :: [Name]
  , body :: AnnotatedExpr a
  }
  deriving Eq
deriving instance Show (Function Void)
deriving instance Show (Function SourcePos)

instance Show (Function ()) where
  show (Function _ name args body) =
    unpack name <> " " <> intercalate " " (map unpack args) <> " = " <> show body

instance Functor Function where
  fmap f (Function annot name args body) = Function (f annot) name args (f <$> body)

-- case name, variables to unpack, body
data CaseAlternative a = Alternative Tag [Name] (AnnotatedExpr a)
  deriving Eq
deriving instance Show (CaseAlternative SourcePos)
deriving instance Show (CaseAlternative Void)
deriving instance Show (CaseAlternative ())

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
  deriving Eq
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

instance Functor AnnotatedExpr where
  fmap f (AnnExprInt a n) = AnnExprInt (f a) n
  fmap f (AnnExprVariable a name) = AnnExprVariable (f a) name
  fmap f (AnnExprApplication a expr1 expr2) = AnnExprApplication (f a) (f <$> expr1) (f <$> expr2)
  fmap f (AnnExprString a s) = AnnExprString (f a) s
  fmap f (AnnExprDouble a d) = AnnExprDouble (f a) d
  fmap f (AnnExprConstructor a tag arity) = AnnExprConstructor (f a) tag arity
  fmap f (AnnExprLet a name binding body) = AnnExprLet (f a) name (f <$> binding) (f <$> body)
  fmap f (AnnExprLambda a name expr) = AnnExprLambda (f a) name (f <$> expr)
  fmap f (AnnExprCase a expr alters) = AnnExprCase (f a) (f <$> expr) (map (\(Alternative n name expr) -> (Alternative n name (f <$> expr))) alters)

instance Show (AnnotatedExpr ()) where
  show (AnnExprVariable _ name) = show name
  show (AnnExprInt _ n) = show n
  show (AnnExprString _ s) = show s
  show (AnnExprDouble _ d) = show d
  show (AnnExprConstructor _ tag arity) = "(Constr " <> show tag <> " " <> show arity <> ")"
  show (AnnExprApplication _ a b) = "(App " <> show a <> " " <> show b <> ")"
  show (AnnExprLet _ name binding body) = "(Let " <> show name <> " " <> show binding <> " " <> show body <> ")"
  show (AnnExprLambda _ name expr) = "(Lam " <> show name <> " " <> show expr <> ")"
  show (AnnExprCase _ expr alts) = "(Case " <> show expr <> " " <> show alts <> ")"

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

instance Show Expr where
  show (ExprVariable name) = show name
  show (ExprInt n) = show n
  show (ExprString s) = show s
  show (ExprDouble d) = show d
  show (ExprConstructor tag arity) = "(Constr " <> show tag <> "," <> show arity <> ")"
  show (ExprApplication a b) = "(App " <> show a <> " " <> show b <> ")"
  show (ExprLet name binding body) = "(Let " <> show name <> " " <> show binding <> " " <> show body <> ")"
  show (ExprLambda name expr) = "(Lam " <> show name <> " " <> show expr <> ")"
  show (ExprCase expr alts) = "(Case " <> show expr <> " " <> show alts <> ")"
  show _ = error "Avoiding `Pattern match(es) are non-exhaustive` due to PatternSynonyms"

annotatedToExpr :: AnnotatedExpr a -> Expr
annotatedToExpr = fmap (const void)

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
