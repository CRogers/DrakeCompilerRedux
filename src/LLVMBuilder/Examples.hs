{-# LANGUAGE RebindableSyntax, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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
	addr <- alloca (SIntTy sn32) $ constant sn32 1
	loaded <- load addr
	plus3 <- add loaded c3
	store addr plus3
	b <- add c3 c3
	x <- add b b
	ret x
	bl <- createBasicBlock "bl"
	switchTo bl
	y <- add b x
	ret y