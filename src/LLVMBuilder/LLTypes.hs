{-# LANGUAGE DataKinds #-}

module LLVMBuilder.LLTypes where

import Data.Singletons

import LLVMBuilder.Types
import LLVMBuilder.Numbers

type I1 = 'IntTy N1
type SI1 = Sing I1

i1 :: SI1
i1 = SIntTy sn1

type I8 = 'IntTy N8
type SI8 = Sing I8

i8 :: SI8
i8 = SIntTy sn8

type I32 = 'IntTy N32
type SI32 = Sing I32

i32 :: SI32
i32 = SIntTy sn32

type BoolTy = I1