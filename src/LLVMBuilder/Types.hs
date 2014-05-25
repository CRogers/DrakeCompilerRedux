{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, GADTs, DataKinds, TypeFamilies #-}

module LLVMBuilder.Types where

import Control.Lens

import qualified LLVM.General.AST as LL
import LLVM.General.AST.Type (FloatingPointFormat)

import Data.Singletons.TH
--import Data.Singletons.TypeLits

newtype BasicBlockRef = BasicBlockRef { _unBasicBlockRef :: LL.Name } deriving (Eq, Show)
makeLenses ''BasicBlockRef

newtype ValueRef = ValueRef { _unValueRef :: LL.Operand } deriving (Eq, Show)
makeLenses ''ValueRef

genSingletons [''FloatingPointFormat]

singletons [d|
	data Nat = Zero | Succ Nat
	data LLVMType
		= VoidTy
		| IntTy Nat
		| PointerTy LLVMType
		| FloatTy Nat FloatingPointFormat
		| FunctionTy LLVMType [LLVMType] Bool
		| VectorTy Nat LLVMType
		| StructTy Nat [LLVMType]
		| ArrayTy Nat LLVMType
 |]