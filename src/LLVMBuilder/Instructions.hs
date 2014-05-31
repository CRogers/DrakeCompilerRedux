{-# LANGUAGE DataKinds, TypeFamilies #-}

module LLVMBuilder.Instructions where

import Data.Singletons.Prelude.List (Sing(..))

import LLVMBuilder.Core
import LLVMBuilder.Types
import LLVMBuilder.Numbers

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

add :: VR t -> VR t -> CBuilder BasicBlock (VR t)
add (SValueRef t x) (SValueRef _ y) = appendInstr t $ LL.Add False False x y []

alloca :: SLLVMType t -> VR ('IntTy N32) -> CBuilder BasicBlock (VR ('PointerTy t))
alloca t (SValueRef _ n) = appendInstr (SPointerTy t) $ LL.Alloca (fromSLLVM t) (Just n) 0 []

load :: VR ('PointerTy t) -> CBuilder BasicBlock (VR t)
load (SValueRef (SPointerTy t) addr) = appendInstr t $ LL.Load False addr Nothing 0 []

store :: VR ('PointerTy t) -> VR t -> CBuilder BasicBlock (VR 'VoidTy)
store (SValueRef _ addr) (SValueRef _ val) = appendInstr SVoidTy $ LL.Store False addr val Nothing 0 []

ret :: VR t -> Builder BasicBlock Terminated ()
ret (SValueRef _ x) = appendTerm $ LL.Ret (Just x) []

br :: BasicBlockRef -> Builder BasicBlock Terminated ()
br (BasicBlockRef n) = appendTerm $ LL.Br n [] 

condBr :: VR BoolTy -> BasicBlockRef -> BasicBlockRef -> Builder BasicBlock Terminated ()
condBr (SValueRef _ cond) (BasicBlockRef true) (BasicBlockRef false) = appendTerm $ LL.CondBr cond true false []
 
constant :: SNat n -> Integer -> VR ('IntTy n)
constant n = SValueRef (SIntTy n) . LL.ConstantOperand . LLC.Int (fromSNat n)