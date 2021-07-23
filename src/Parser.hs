{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AST

type Parser = Parsec Void Text

prog :: Parser Expr
prog = scn *> expr <* eof

expr :: Parser Expr
expr = chainl1 term exprOp ExprTerm

term :: Parser Term
term = chainl1 atom termOp TermAtom

atom =  TermNumber <$> num
    <|> TermExpr <$> ( char '(' *> expr <* char ')' )

num :: Parser Number
num = L.lexeme scn $ Number <$> L.decimal

exprOp :: Parser (Expr -> Term -> Expr)
exprOp = L.lexeme scn $ choice
  [ (\a b -> Expr a Add b) <$ char '+'
  , (\a b -> Expr a Sub b) <$ char '-'
  ]

termOp :: Parser (Term -> Atom -> Term)
termOp = L.lexeme scn $ choice
  [ (\a b -> Term a Mul b) <$ char '*'
  , (\a b -> Term a Div b) <$ char '/'
  ]

scn :: Parser ()
scn = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

chainl1 :: Parser b -> Parser (a -> b -> a) -> (b -> a) -> Parser a
chainl1 p op c = do r <- p
                    h <- helper
                    return (h (c r))
  where
    -- helper :: Parser (a -> a)
    helper = try $ do o <- op
                      r <- p
                      h <- helper
                      return (\r' -> h (o r' r))
             <|> return id



          
      
      
    
