{-# LANGUAGE RebindableSyntax, DataKinds #-}

module LLVMBuilder.Examples where

import Prelude hiding (Monad(..))
import IxMonadSyntax

import LLVMBuilder.Builder

c3 :: ValueRef ('IntTy N32)
c3 = constant sn32 3

test :: Builder Setup Terminated ()
test = do
	entry <- createBasicBlock "entry"
	switchTo entry
	alloca (SIntTy sn32) $ constant sn32 1
	b <- add c3 c3
	x <- add b b
	ret x
	bl <- createBasicBlock "bl"
	switchTo bl
	y <- add b x
	ret y