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

type BoolTy = 'IntTy N1

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

type P0 n = Succ n
type P1 n = P0 (P0 n)
type P2 n = P1 (P1 n)
type P3 n = P2 (P2 n)
type P4 n = P3 (P3 n)
type P5 n = P4 (P4 n)

type N1 = Succ Zero
type SN1 = Sing N1

type N16  = P4 Zero
type SN16 = Sing N16

type N32 = P5 Zero
type SN32 = Sing N32

p0 n = SSucc n
p1 n = p0 (p0 n)
p2 n = p1 (p1 n)
p3 n = p2 (p2 n)
p4 n = p3 (p3 n)
p5 n = p4 (p4 n)

sn16 :: SN16
sn16 = p4 SZero

sn32 :: SN32
sn32 = p5 SZero

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Succ (toNat $ n - 1)