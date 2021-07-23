

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
      (Right x) -> x `shouldBe` (ExprTerm (TermFactor (FactorNumber (Number 1))))

  it "production" $ do
    let r = parse expr "" (pack "1*2")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                  (Term
                                    (TermFactor (FactorNumber (Number 1)))
                                    Mul
                                    (FactorNumber (Number 2))
                                  ))
  it "production2" $ do
    let r = parse expr "" (pack "1*2*3")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                  (Term
                                    (Term
                                     (TermFactor (FactorNumber (Number 1)))
                                     Mul
                                     (FactorNumber (Number 2))
                                    )
                                    Mul
                                    (FactorNumber (Number 3))
                                  ))

  it "addition" $ do
    let r = parse expr "" (pack "1+2")
    case r of
      (Right x) -> x `shouldBe` (Expr
                                  (ExprTerm
                                    (TermFactor (FactorNumber (Number 1))))
                                  Add
                                  (TermFactor (FactorNumber (Number 2)))
                                )

  it "TermExpr" $ do
    let r = parse expr "" (pack "(1+2)")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                 (TermFactor
                                  (FactorExpr
                                   (Expr
                                    (ExprTerm
                                     (TermFactor (FactorNumber (Number 1))))
                                    Add
                                    (TermFactor (FactorNumber (Number 2)))
                                   ))
                                 ))
                   


