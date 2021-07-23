
module AST where

data Expr
  = Expr Expr ExprOp Term
  | ExprTerm Term
  deriving (Show,Eq)
data Term
  = Term Term TermOp Atom
  | TermAtom Atom
  deriving (Show,Eq)
data Atom
  = TermNumber Number
  | TermExpr   Expr
  deriving (Show,Eq)
data Number = Number Integer deriving (Show,Eq)

data ExprOp = Add | Sub deriving (Show,Eq)
data TermOp = Mul | Div deriving (Show,Eq)

