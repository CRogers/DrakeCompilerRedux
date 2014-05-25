module LLVMBuilder.Instructions where

import LLVMBuilder.Core
import LLVMBuilder.Types

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

add :: ValueRef -> ValueRef -> CBuilder BasicBlock ValueRef
add (ValueRef x) (ValueRef y) = appendInstr $ LL.Add False False x y []

--alloca :: LL.Type -> CBuilder BasicBlock ValueRef
--alloca t = appendInstr $ LL.Alloca

ret :: ValueRef -> Builder BasicBlock Terminated ()
ret (ValueRef x) = appendTerm $ LL.Ret (Just x) []

br :: BasicBlockRef -> Builder BasicBlock Terminated ()
br (BasicBlockRef n) = appendTerm $ LL.Br n [] 

condBr :: ValueRef -> BasicBlockRef -> BasicBlockRef -> Builder BasicBlock Terminated ()
condBr (ValueRef cond) (BasicBlockRef true) (BasicBlockRef false) = appendTerm $ LL.CondBr cond true false []
 
constant :: Integer -> ValueRef
constant = ValueRef . LL.ConstantOperand . LLC.Int 32