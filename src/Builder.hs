{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Builder where

import Control.Applicative (Applicative)
import Control.Monad (void)
import Control.Monad.State (MonadState, State, get, put, runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Word (Word)

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC
import qualified LLVM.General.AST.Global as LLG

data FState = FState (Map LL.Name (Maybe (BasicBlockBuilder ()))) LL.Global

newtype FunctionBuilder a = FunctionBuilder (State FState a)
	deriving (Functor, Monad, Applicative, MonadState FState)

onlyJusts :: [(a, Maybe b)] -> [(a, b)]
onlyJusts = map (\(a, Just b) -> (a, b)) . filter (isJust . snd)

runFunctionBuilder :: FunctionBuilder a -> String -> LL.Global
runFunctionBuilder (FunctionBuilder s) nameStr = f { LLG.name = name, LLG.returnType = LL.IntegerType 32, LLG.basicBlocks = bbs }
	where name = LL.Name nameStr
	      initialState = FState M.empty LLG.functionDefaults
	      (FState bbbs f) = snd $ runState s initialState
	      g (c, bbList) (n, bbb) = let (c', bb) = runBasicBlockBuilder bbb n c in (c', bbList ++ [bb])
	      bbs = snd $ foldl g (0, []) $ onlyJusts $ M.toList bbbs

setParameters :: [(LL.Type, String)] -> FunctionBuilder ()
setParameters ps = do
	(FState bbbs f) <- get
	let params = map (\(t,n) -> LL.Parameter t (LL.Name n) []) ps
	let f' = f { LLG.parameters = (params, False) }
	put $ FState bbbs f'

newtype BasicBlockRef = BasicBlockRef LL.Name

createBasicBlockRef :: String -> FunctionBuilder BasicBlockRef
createBasicBlockRef n = do
	let name = LL.Name n
	(FState bbbs f) <- get
	if M.member name bbbs then error $ "Basic block " ++ n ++ " already exists"
	else do
		put $ FState (M.insert name Nothing bbbs) f
		return $ BasicBlockRef name

addToBasicBlock :: BasicBlockRef -> BasicBlockBuilder a -> FunctionBuilder ()
addToBasicBlock (BasicBlockRef name) bbb' = do
	(FState bbbs f) <- get
	let bbb = void bbb'
	case M.lookup name bbbs of
		Nothing -> error $ "No refernce for " ++ show name
		Just (Just oldBBB) ->
			let newBBB = void $ oldBBB >> bbb in
			put $ FState (M.insert name (Just newBBB) bbbs) f
		Just Nothing -> do
			put $ FState (M.insert name (Just $ void bbb) bbbs) f

basicBlock :: String -> BasicBlockBuilder () -> FunctionBuilder ()
basicBlock n bbb = createBasicBlockRef n >>= flip addToBasicBlock bbb

ftest :: FunctionBuilder ()
ftest = do
	cat <- createBasicBlockRef "cat"
	hat <- createBasicBlockRef "hat"
	addToBasicBlock cat $ do
		a <- add c3 c3
		add c3 a
	addToBasicBlock hat $ do
		a <- add c3 c3
		ret a
	addToBasicBlock cat $ do
		ret c3


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