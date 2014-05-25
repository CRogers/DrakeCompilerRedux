{-# LANGUAGE RebindableSyntax #-}

module LLVMBuilder.IxLens where

import Prelude hiding (Monad(..))
import IxMonadSyntax

import Control.Lens
import IxState

infix 4 .==, %==

(.==) :: IxMonadState m => ASetter s t a b -> b -> m s t ()
l .== b = imodify (l .~ b)   

(%==) :: (Profunctor p, IxMonadState m) => Setting p s t a b -> p a b -> m s t ()
l %== b = imodify (l %~ b)

ixuse :: IxMonadState m => Getting a s a -> m s s a
ixuse l = do
	x <- iget
	return $ x ^. l