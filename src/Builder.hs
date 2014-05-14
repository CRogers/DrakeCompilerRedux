{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Builder where

import Control.Applicative (Applicative)
import Control.Monad.State (MonadState, State, get, put, runState)
import Data.Word (Word)

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

data Finished
data Ongoing

data BState = BState {
	instrs :: [LL.Named LL.Instruction],
	term :: (Maybe (LL.Named LL.Terminator)),
	counter :: Word
}	deriving (Show)

newtype BasicBlockBuilder a = BasicBlockBuilder (State BState a)
	deriving (Functor, Monad, Applicative, MonadState BState)

runBasicBlockBuilder :: BasicBlockBuilder a -> Word -> LL.BasicBlock
runBasicBlockBuilder (BasicBlockBuilder s) i = LL.BasicBlock n is t
	where n = LL.UnName i
	      initialState = BState [] Nothing (i + 1)
	      (BState is (Just t) _) = snd $ runState s initialState

appendInstr :: LL.Instruction -> BasicBlockBuilder LL.Operand
appendInstr instr = do
	(BState is t c) <- get
	let n = LL.UnName c
	let i = n LL.:= instr
	put $ BState (is ++ [i]) t (c + 1)
	return $ LL.LocalReference n

appendTerm :: LL.Terminator -> BasicBlockBuilder ()
appendTerm t = do
	(BState is _ c) <- get
	put $ BState is (Just $ LL.Do t) c

add :: LL.Operand -> LL.Operand -> BasicBlockBuilder LL.Operand
add x y = appendInstr $ LL.Add False False x y []

ret :: Maybe LL.Operand -> BasicBlockBuilder ()
ret x = appendTerm $ LL.Ret x []

c3 :: LL.Operand
c3 = (LL.ConstantOperand (LLC.Int 32 3))

test = do
	b <- add c3 c3
	x <- add b b
	y <- add b x
	ret $ Just y