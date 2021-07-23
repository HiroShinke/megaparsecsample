
module AST where

data Expr
  = Expr Expr ExprOp Term
  | ExprTerm Term
  deriving (Show)
data Term
  = Term Term TermOp Number
  | TermNumber Number
  | TermExpr   Expr
  deriving (Show)
data Number = Number Integer deriving (Show)

data ExprOp = Add | Sub deriving (Show)
data TermOp = Mul | Div deriving (Show)

