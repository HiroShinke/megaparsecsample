
module AST where

data Expr
  = Expr Expr ExprOp Term
  | ExprTerm Term
  deriving (Show,Eq)
data Term
  = Term Term TermOp Factor
  | TermFactor Factor
  deriving (Show,Eq)
data Factor
  = FactorNumber Number
  | FactorExpr   Expr
  deriving (Show,Eq)
data Number = Number Integer deriving (Show,Eq)

data ExprOp = Add | Sub deriving (Show,Eq)
data TermOp = Mul | Div deriving (Show,Eq)

