{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Builder where

import Control.Applicative (Applicative)
import Control.Monad (void)
import Control.Monad.State (MonadState, State, get, put, modify, runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Word (Word)

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC
import qualified LLVM.General.AST.Global as LLG

data FState = FState (Map LL.Name (BasicBlockBuilder ())) [LL.Name] BasicBlockRef LL.Global

newtype FunctionBuilder a = FunctionBuilder (State FState a)
	deriving (Functor, Monad, Applicative, MonadState FState)

emptyBBB :: BasicBlockBuilder ()
emptyBBB = return ()

runFunctionBuilder :: FunctionBuilder a -> String -> LL.Global
runFunctionBuilder (FunctionBuilder s) nameStr = f { LLG.name = name, LLG.returnType = LL.IntegerType 32, LLG.basicBlocks = bbs }
	where name = LL.Name nameStr
	      entry = LL.Name "entry"
	      initialMap = M.fromList [(entry, emptyBBB)]
	      initialState = FState initialMap [entry] (BasicBlockRef entry) LLG.functionDefaults
	      (FState bbbs ord _ f) = snd $ runState s initialState
	      g (c, bbList) (n, bbb) = let (c', bb) = runBasicBlockBuilder bbb n c in (c', bbList ++ [bb])
	      bbs = snd $ foldl g (0, []) $ map (\k -> (k, fromJust $ M.lookup k bbbs)) ord

setParameters :: [(LL.Type, String)] -> FunctionBuilder [LL.Operand]
setParameters ps = do
	(FState bbbs ord cur f) <- get
	let params = map (\(t,n) -> LL.Parameter t (LL.Name n) []) ps
	let f' = f { LLG.parameters = (params, False) }
	put $ FState bbbs ord cur f'
	return $ map (LL.LocalReference . LL.Name . snd) ps

newtype BasicBlockRef = BasicBlockRef LL.Name

createBasicBlockRef :: String -> FunctionBuilder BasicBlockRef
createBasicBlockRef n = do
	let name = LL.Name n
	(FState bbbs ord cur f) <- get
	if M.member name bbbs then error $ "Basic block " ++ n ++ " already exists"
	else do
		put $ FState (M.insert name emptyBBB bbbs) (ord ++ [name]) cur f
		return $ BasicBlockRef name

addToBasicBlock :: BasicBlockRef -> BasicBlockBuilder a -> FunctionBuilder ()
addToBasicBlock (BasicBlockRef name) bbb = do
	(FState bbbs ord cur f) <- get
	case M.lookup name bbbs of
		Nothing -> error $ "No refernce for " ++ show name
		Just oldBBB ->
			let newBBB = void $ oldBBB >> bbb in
			put $ FState (M.insert name newBBB bbbs) ord cur f

switchTo :: BasicBlockRef -> FunctionBuilder ()
switchTo ref = modify (\(FState bbbs ord _ f) -> FState bbbs ord ref f)

getCurrent :: FunctionBuilder BasicBlockRef
getCurrent = do
	(FState _ _ cur _) <- get
	return cur

addToCurrent :: BasicBlockBuilder a -> FunctionBuilder ()
addToCurrent bbb = do
	ref <- getCurrent
	addToBasicBlock ref bbb

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

condBr :: LL.Operand -> BasicBlockRef -> BasicBlockRef -> BasicBlockBuilder ()
condBr cond (BasicBlockRef true) (BasicBlockRef false) = appendTerm $ LL.CondBr cond true false []
 
c3 :: LL.Operand
c3 = constant 3

test :: BasicBlockBuilder ()
test = do
	b <- add c3 c3
	x <- add b b
	y <- add b x
	ret y