{-# LANGUAGE NoImplicitPrelude #-}

module IxMonadSyntax where

import Prelude (error, String, Bool)
import Control.Monad.Indexed ((>>>=),IxPointed(..),IxMonad)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
a >> b = a >>>= \_ -> b

return :: IxPointed m => a -> m i i a
return = ireturn

fail :: IxMonad m => String -> m i i a
fail = error

ifThenElse :: Bool -> t -> t -> t
ifThenElse cond x y = if cond then x else y