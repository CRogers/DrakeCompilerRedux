{-# LANGUAGE StandaloneDeriving, FlexibleInstances, ScopedTypeVariables, TypeOperators, UndecidableInstances, TemplateHaskell, GADTs, DataKinds, ConstraintKinds, TypeFamilies #-}
--{-# OPTIONS_GHC -ddump-splices #-}

module LLVMBuilder.Types where

import qualified LLVM.General.AST as LL
import LLVM.General.AST.Type (FloatingPointFormat)

import Data.Singletons.TH

newtype BasicBlockRef = BasicBlockRef { unBasicBlockRef :: LL.Name } deriving (Eq, Show)

--genSingletons [''FloatingPointFormat]

singletons [d|
	data Nat = Zero | Succ Nat deriving (Eq, Show)

	data LLVMType
		= VoidTy Nat
		| IntTy Nat
		| PointerTy LLVMType
		{-| FloatTy Nat --FloatingPointFormat
		| FunctionTy LLVMType [LLVMType] Bool
		| VectorTy Nat LLVMType
		| StructTy Nat [LLVMType]
		| ArrayTy Nat LLVMType-}
		deriving (Show)
 |]

data ValueRef :: LLVMType -> * where
	ValueRef :: LL.Operand -> SLLVMType t -> ValueRef t

instance Show (ValueRef t) where
	show (ValueRef op ty) = "ValueRef (" ++ show op ++ ") (" ++ show (fromSing ty) ++ ")"

type P1 n = Succ n
type P2 n = P1 (P1 n)
type P3 n = P2 (P2 n)
type P4 n = P3 (P3 n)
type P5 n = P4 (P4 n)

type N32 = P5 Zero
type SN32 = Sing N32

n1 n = SSucc n
n2 n = n1 (n1 n)
n3 n = n2 (n2 n)
n4 n = n3 (n3 n)
n5 n = n4 (n4 n)

sn32 :: SN32
sn32 = n5 SZero

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Succ (toNat $ n - 1)