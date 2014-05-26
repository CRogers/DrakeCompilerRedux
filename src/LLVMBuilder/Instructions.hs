{-# LANGUAGE DataKinds #-}

module LLVMBuilder.Instructions where

import LLVMBuilder.Core
import LLVMBuilder.Types

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

add :: ValueRef t -> ValueRef t -> CBuilder BasicBlock (ValueRef t)
add (ValueRef t x) (ValueRef _ y) = appendInstr t $ LL.Add False False x y []

alloca :: SLLVMType t -> ValueRef ('IntTy N32) -> CBuilder BasicBlock (ValueRef ('PointerTy t))
alloca t (ValueRef _ n) = appendInstr (SPointerTy t) $ LL.Alloca (fromSLLVM t) (Just n) 0 []

ret :: ValueRef t -> Builder BasicBlock Terminated ()
ret (ValueRef _ x) = appendTerm $ LL.Ret (Just x) []

br :: BasicBlockRef -> Builder BasicBlock Terminated ()
br (BasicBlockRef n) = appendTerm $ LL.Br n [] 

condBr :: ValueRef BoolTy -> BasicBlockRef -> BasicBlockRef -> Builder BasicBlock Terminated ()
condBr (ValueRef _ cond) (BasicBlockRef true) (BasicBlockRef false) = appendTerm $ LL.CondBr cond true false []
 
constant :: SNat n -> Integer -> ValueRef ('IntTy n)
constant n = ValueRef (SIntTy n) . LL.ConstantOperand . LLC.Int (fromSNat n)