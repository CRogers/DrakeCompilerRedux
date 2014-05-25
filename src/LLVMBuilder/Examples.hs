{-# LANGUAGE RebindableSyntax #-}

module Examples where

import Prelude hiding (Monad(..))
import IxMonadSyntax

import LLVMBuilder.Builder

c3 :: ValueRef
c3 = constant 3

test :: Builder Setup Terminated ()
test = do
	entry <- createBasicBlock "entry"
	switchTo entry
	b <- add c3 c3
	x <- add b b
	ret x
	bl <- createBasicBlock "bl"
	switchTo bl
	y <- add b x
	ret y