{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module IxState where

import Control.Applicative (Applicative(..))
import Control.Monad.Indexed (IxApplicative(..),IxFunctor(..),IxPointed(..),IxMonad(..),(>>>=))
import Control.Monad.State.Class (MonadState(..))

newtype IxState i o a = IxState { runIxState :: i -> (a, o) }

instance IxFunctor IxState where
	imap f s = IxState $ \i -> let (a, j) = runIxState s i in (f a, j)

instance IxPointed IxState where
	ireturn x = IxState (x,)

instance IxApplicative IxState where
	iap s t = IxState $ \i -> let (fab, j) = runIxState s i in
	                          let (a  , k) = runIxState t j in
	                          (fab a, k)

instance IxMonad IxState where
	ibind f mx = IxState $ \i -> let (a, j) = runIxState mx i in runIxState (f a) j

class IxMonad m => IxMonadState m where
	iget :: m i i i
	iput :: s -> m i s ()

instance IxMonadState IxState where
	iget = IxState $ \i -> (i, i)
	iput s = IxState $ \_ -> ((),s)

imodify :: IxMonadState m => (s -> t) -> m s t ()
imodify f = iget >>>= (\s -> iput $ f s)

instance Functor (IxState i j) where
	fmap = imap

instance Applicative (IxState i i) where
	pure = ireturn
	(<*>) = iap

instance Monad (IxState i i) where
	return = ireturn
	m >>= k = ibind k m

instance MonadState i (IxState i i) where
	get = iget
	put = iput