

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

  let factorNumber n = FactorNumber (Number n)
  let termFactor n = (TermFactor (factorNumber n))

  it "basic" $ do
    let r = parse expr "" (pack "1")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm (termFactor 1))

  it "production" $ do
    let r = parse expr "" (pack "1*2")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                  (Term
                                   (termFactor 1) Mul (factorNumber 2)
                                  ))
  it "production2" $ do
    let r = parse expr "" (pack "1*2*3")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                  (Term
                                    (Term
                                     (termFactor 1) Mul (factorNumber 2)
                                    )
                                    Mul
                                    (factorNumber 3)
                                  ))

  it "addition" $ do
    let r = parse expr "" (pack "1+2")
    case r of
      (Right x) -> x `shouldBe` (Expr
                                  (ExprTerm
                                   (termFactor 1)) Add (termFactor 2)
                                )
  let termFactor12 = (TermFactor 
                     (FactorExpr
                       (Expr
                         (ExprTerm
                           (termFactor 1)) Add (termFactor 2)
                       )))
  it "TermExpr" $ do
    let r = parse expr "" (pack "(1+2)")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                 termFactor12
                                )
                   
  it "TermExpr" $ do
    let r = parse expr "" (pack "(1+2)*2")
    case r of
      (Right x) -> x `shouldBe` (ExprTerm
                                 (Term
                                  termFactor12 Mul (factorNumber 2)
                                 ))


