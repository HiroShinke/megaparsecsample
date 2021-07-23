

module ParserSpec where

import Parser
import AST
import Text.Megaparsec (parse) 
import Data.Text (pack)
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except

spec :: Spec
spec = 
  describe "test case" $ do

  it "basic" $ do
    let r = parse expr "" (pack "1")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm (TermAtom (TermNumber (Number 1))))

  it "production" $ do
    let r = parse expr "" (pack "1*2")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                  (Term
                                    (TermAtom (TermNumber (Number 1)))
                                    Mul
                                    (TermNumber (Number 2))
                                  ))
  it "production2" $ do
    let r = parse expr "" (pack "1*2*3")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                  (Term
                                    (Term
                                     (TermAtom (TermNumber (Number 1)))
                                     Mul
                                     (TermNumber (Number 2))
                                    )
                                    Mul
                                    (TermNumber (Number 3))
                                  ))

  it "addition" $ do
    let r = parse expr "" (pack "1+2")
    case r of
      (Right x) -> x `shouldBe` (Expr
                                  (ExprTerm
                                    (TermAtom (TermNumber (Number 1))))
                                  Add
                                  (TermAtom (TermNumber (Number 2)))
                                )

  it "TermExpr" $ do
    let r = parse expr "" (pack "(1+2)")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                 (TermAtom
                                  (TermExpr
                                   (Expr
                                    (ExprTerm
                                     (TermAtom (TermNumber (Number 1))))
                                    Add
                                    (TermAtom (TermNumber (Number 2)))
                                   ))
                                 ))
                   


