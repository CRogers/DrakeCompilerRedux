{-# LANGUAGE DataKinds, TypeFamilies #-}

module LLVMBuilder.Instructions where

import LLVMBuilder.Core
import LLVMBuilder.Types
import LLVMBuilder.Numbers
import LLVMBuilder.LLTypes

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

add :: VR t -> VR t -> CBuilder BasicBlock ps r (VR t)
add (SValueRef t x) (SValueRef _ y) = appendInstr t $ LL.Add False False x y []

alloca :: SLLVMType t -> VR ('IntTy N32) -> CBuilder BasicBlock ps r (VR ('PointerTy t))
alloca t (SValueRef _ n) = appendInstr (SPointerTy t) $ LL.Alloca (fromSLLVM t) (Just n) 0 []

load :: VR ('PointerTy t) -> CBuilder BasicBlock ps r (VR t)
load (SValueRef (SPointerTy t) addr) = appendInstr t $ LL.Load False addr Nothing 0 []

store :: VR ('PointerTy t) -> VR t -> CBuilder BasicBlock ps r (VR 'VoidTy)
store (SValueRef _ addr) (SValueRef _ val) = appendInstr SVoidTy $ LL.Store False addr val Nothing 0 []

ret :: VR r -> IBuilder BasicBlock Terminated ps r ()
ret (SValueRef _ x) = appendTerm $ LL.Ret (Just x) []

br :: BasicBlockRef -> IBuilder BasicBlock Terminated ps r ()
br (BasicBlockRef n) = appendTerm $ LL.Br n [] 

condBr :: VR BoolTy -> BasicBlockRef -> BasicBlockRef -> IBuilder BasicBlock Terminated ps r ()
condBr (SValueRef _ cond) (BasicBlockRef true) (BasicBlockRef false) = appendTerm $ LL.CondBr cond true false []
 
constant :: SNat n -> Integer -> VR ('IntTy n)
constant n = SValueRef (SIntTy n) . LL.ConstantOperand . LLC.Int (fromSNat n)