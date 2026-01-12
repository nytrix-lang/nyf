{-# LANGUAGE DeriveGeneric #-}

module AST where

import Data.Text (Text)
import GHC.Generics (Generic)

--- Top-level declarations
data Decl
  = UseDecl Text          -- use std.core
  | UseAsDecl Text [Text] -- use std.math
  | DefineDecl Text Expr  -- define x = expr
  | FnDecl Text [Text] (Maybe DocString) [Stmt]
  deriving (Show, Eq, Generic)

type DocString = Text

--- Statements

data Stmt
  = ExprStmt Expr
  | Assign Text Expr                  -- x = expr
  | IfStmt Expr [Stmt] (Maybe [Stmt]) -- if (cond) {..} else {..}
  | WhileStmt Expr [Stmt]
  | ReturnStmt (Maybe Expr)
  deriving (Show, Eq, Generic)

--- Expressions

data Expr
  = Var Text
  | IntLit Integer
  | FloatLit Double
  | StrLit Text
  | Call Text [Expr]
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | Parens Expr
  deriving (Show, Eq, Generic)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or
  deriving (Show, Eq, Generic)

data UnOp
  = Neg | Not
  deriving (Show, Eq, Generic)
