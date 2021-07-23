{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler where

import Data.Text.Internal.Lazy
import Data.Functor.Identity

import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.AST.Constant as C
import LLVM.AST.IntegerPredicate as P

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant

import qualified AST

type LLVMBuilder = IRBuilderT (ModuleBuilderT Identity)

compile :: AST.Expr -> Text
compile expr = ppllvm $ buildModule "main" $ mdo
  form <- globalStringPtr "%d\n" "putNumForm"
  printf <- externVarArgs "printf" [ptr i8] i32
  function "main" [] i32 $ \[] -> mdo
    entry <- block `named` "entry"; mdo
      r <- toOperand expr
      call printf [(ConstantOperand form, []), (r, [])]              
      ret (int32 0)

class LLVMOperand a where
  toOperand :: a -> LLVMBuilder Operand

instance LLVMOperand AST.Expr where
  toOperand (AST.ExprTerm e) = toOperand e
  toOperand (AST.Expr t AST.Add e) = mdo
    t' <- toOperand t
    e' <- toOperand e
    add t' e'
  toOperand (AST.Expr t AST.Sub e) = mdo
    t' <- toOperand t
    e' <- toOperand e
    sub t' e'

instance LLVMOperand AST.Term where
  toOperand (AST.TermFactor n) = toOperand n
  toOperand (AST.Term n AST.Mul t) = mdo
    n' <- toOperand n
    t' <- toOperand t
    mul n' t'
  toOperand (AST.Term n AST.Div t) = mdo
    n' <- toOperand n
    t' <- toOperand t
    sdiv n' t'

instance LLVMOperand AST.Factor where
  toOperand (AST.FactorNumber n) = toOperand n
  toOperand (AST.FactorExpr   e) = toOperand e

instance LLVMOperand AST.Number where
  toOperand (AST.Number n) = return (int32 n)
