{-# LANGUAGE RebindableSyntax, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module LLVMBuilder.Examples where

import Prelude hiding (Monad(..))
import IxMonadSyntax

import Data.Singletons.Prelude.List (Sing(..), sHead)

import LLVMBuilder.Builder

c3 :: VR I32
c3 = constant sn32 3

test :: Builder Setup Terminated ()
test = do
	ps <- setParameters $ SCons (SParam "cat" i8) SNil
	let cat = sHead ps

	entry <- createBasicBlock "entry"

	switchTo entry
	addr <- alloca i32 $ constant sn32 1
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