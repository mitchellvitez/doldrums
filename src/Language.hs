{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Language where

import Control.DeepSeq (NFData)
import Data.Text (Text, pack, intercalate)
import GHC.Generics
import Text.Megaparsec (SourcePos)

-- a Program is a list of top-level definitions
type Program = [TopLevelDefn]
type TopLevelDefn = (Name, [Name], Annotated Expr) -- ^ name, list of arguments, body

type Name = Text
type Tag = Int
type Arity = Int

-- Could replace SourcePos with a more-complex Annotation type if more info exists
data Annotated e = Annotated SourcePos e
  deriving (Eq, Show)

-- The meat of the (untyped) AST
data Expr
  = ExprVariable Name
  | ExprInt Integer
  | ExprBool Bool
  | ExprString Text
  | ExprDouble Double
  | ExprConstructor Tag Arity
  | ExprApplication Expr Expr
  | ExprLet [(Name, Expr)] Expr -- definitions, body
  | ExprLambda [Name] Expr
  deriving (Show, Eq, Ord, Generic, NFData)
