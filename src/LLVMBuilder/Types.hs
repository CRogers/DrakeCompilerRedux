{-# LANGUAGE StandaloneDeriving, FlexibleInstances, ScopedTypeVariables, TypeOperators, UndecidableInstances, TemplateHaskell, GADTs, DataKinds, ConstraintKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LLVMBuilder.Types where

import Data.Word

import qualified LLVM.General.AST as LL
import LLVM.General.AST.AddrSpace (AddrSpace(..))

import Data.Singletons.TH
import Data.Singletons.Prelude.List

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

data Parameter = Param LLVMType deriving Show
data ParameterTerm = ParamTerm String LLVMType deriving Show

data instance Sing (x :: Parameter) where
	SParam :: String -> Sing x -> Sing (Param x)

type SParameter (x :: Parameter) = Sing x

instance SingKind ('KProxy :: KProxy Parameter) where
  type DemoteRep ('KProxy :: KProxy Parameter) = ParameterTerm
  fromSing (SParam s t) = ParamTerm s (fromSing t)
  toSing (ParamTerm s t) = case toSing t of
               SomeSing t' -> SomeSing (SParam s t')

genDefunSymbols [''Parameter]

promoteOnly [d|
	parameterToLLVMType :: Parameter -> LLVMType
	parameterToLLVMType (Param t) = t

	parametersToLLVMTypes :: [Parameter] -> [LLVMType]
	parametersToLLVMTypes = map parameterToLLVMType
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