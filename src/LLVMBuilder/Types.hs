{-# LANGUAGE StandaloneDeriving, FlexibleInstances, ScopedTypeVariables, TypeOperators, UndecidableInstances, TemplateHaskell, GADTs, DataKinds, ConstraintKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LLVMBuilder.Types where

import Data.Word

import qualified LLVM.General.AST as LL
import LLVM.General.AST.AddrSpace (AddrSpace(..))

import Data.Singletons.TH

newtype BasicBlockRef = BasicBlockRef { unBasicBlockRef :: LL.Name } deriving (Eq, Show)

--genSingletons [''FloatingPointFormat]

singletons [d|
	data Nat = Zero | Succ Nat deriving (Eq, Show)

	data LLVMType
		= VoidTy
		| IntTy Nat
		| PointerTy LLVMType
		{-| FloatTy Nat --FloatingPointFormat
		| FunctionTy LLVMType [LLVMType] Bool
		| VectorTy Nat LLVMType
		| StructTy Nat [LLVMType]
		| ArrayTy Nat LLVMType-}
		deriving (Show)
 |]

type BoolTy = 'IntTy ('Succ 'Zero)

fromNat :: Nat -> Word32
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

fromSNat :: SNat n -> Word32
fromSNat = fromNat . fromSing

fromLLVM :: LLVMType -> LL.Type
fromLLVM VoidTy = LL.VoidType
fromLLVM (IntTy n) = LL.IntegerType $ fromNat n
fromLLVM (PointerTy t) = LL.PointerType (fromLLVM t) (AddrSpace 0)

fromSLLVM :: SLLVMType t -> LL.Type
fromSLLVM = fromLLVM . fromSing

data ValueRef :: LLVMType -> * where
	ValueRef :: SLLVMType t -> LL.Operand -> ValueRef t

instance Show (ValueRef t) where
	show (ValueRef ty op) = "ValueRef (" ++ show (fromSing ty) ++ ") (" ++ show op ++ ")"