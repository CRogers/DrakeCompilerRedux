{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Builder where

import Control.Applicative (Applicative)
import Control.Monad.State (MonadState, State, get, put, runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Word (Word)

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC
import qualified LLVM.General.AST.Global as LLG

data FState = FState (Map LL.Name (Maybe LL.BasicBlock)) Word
	deriving Show

newtype FunctionBuilder a = FunctionBuilder (State FState a)
	deriving (Functor, Monad, Applicative, MonadState FState)

runFunctionBuilder :: FunctionBuilder a -> String -> LL.Global
runFunctionBuilder (FunctionBuilder s) n = LL.functionDefaults { LLG.name = name, LLG.returnType = LL.IntegerType 32, LLG.basicBlocks = bbList }
	where name = LL.Name n
	      initialState = FState M.empty 0
	      (FState bbs _) = snd $ runState s initialState
	      bbList = catMaybes $ map snd $ M.toList bbs

newtype BasicBlockRef = BasicBlockRef LL.Name

createBasicBlockRef :: String -> FunctionBuilder BasicBlockRef
createBasicBlockRef n = do
	let name = LL.Name n
	(FState bbs c) <- get
	if M.member name bbs then error $ "Basic block " ++ n ++ " already exists"
	else do
		put $ FState (M.insert name Nothing bbs) c
		return $ BasicBlockRef name

buildBasicBlock :: BasicBlockRef -> BasicBlockBuilder () -> FunctionBuilder ()
buildBasicBlock (BasicBlockRef name) bbb = do
	(FState bbs c) <- get
	case M.lookup name bbs of
		Nothing -> error $ "No refernce for " ++ show name
		Just (Just _) -> error $ "Already build basic block for " ++ show name
		Just Nothing -> do
			let (c', bb) = runBasicBlockBuilder bbb name c
			put $ FState (M.insert name (Just bb) bbs) c'


ftest :: FunctionBuilder ()
ftest = do
	cat <- createBasicBlockRef "cat"
	hat <- createBasicBlockRef "hat"
	buildBasicBlock cat $ do
		a <- add c3 c3
		add c3 a
		br hat
	buildBasicBlock hat $ do
		a <- add c3 c3
		ret a


data BState = BState [LL.Named LL.Instruction] (Maybe (LL.Named LL.Terminator)) Word
	deriving (Show)

newtype BasicBlockBuilder a = BasicBlockBuilder (State BState a)
	deriving (Functor, Monad, Applicative, MonadState BState)

runBasicBlockBuilder :: BasicBlockBuilder a -> LL.Name -> Word -> (Word, LL.BasicBlock)
runBasicBlockBuilder (BasicBlockBuilder s) n i = (finalCount, bb)
	where initialState = BState [] Nothing i
	      (BState is (Just t) finalCount) = snd $ runState s initialState
	      bb = LL.BasicBlock n is t

appendInstr :: LL.Instruction -> BasicBlockBuilder LL.Operand
appendInstr instr = do
	(BState is t c) <- get
	let n = LL.UnName c
	let i = n LL.:= instr
	put $ BState (is ++ [i]) t (c + 1)
	return $ LL.LocalReference n

constant :: Integer -> LL.Operand
constant i = LL.ConstantOperand $ LLC.Int 32 i

appendTerm :: LL.Terminator -> BasicBlockBuilder ()
appendTerm t = do
	(BState is _ c) <- get
	put $ BState is (Just $ LL.Do t) c

add :: LL.Operand -> LL.Operand -> BasicBlockBuilder LL.Operand
add x y = appendInstr $ LL.Add False False x y []

ret :: LL.Operand -> BasicBlockBuilder ()
ret x = appendTerm $ LL.Ret (Just x) []

br :: BasicBlockRef -> BasicBlockBuilder ()
br (BasicBlockRef n) = appendTerm $ LL.Br n [] 

c3 :: LL.Operand
c3 = constant 3

test :: BasicBlockBuilder ()
test = do
	b <- add c3 c3
	x <- add b b
	y <- add b x
	ret y