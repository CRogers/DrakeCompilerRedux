{-# LANGUAGE StandaloneDeriving, TemplateHaskell, RebindableSyntax, GeneralizedNewtypeDeriving, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module Builder where

import Prelude hiding (Monad(..))
import qualified Prelude as Prelude
import IxMonadSyntax

import Control.Lens
import Control.Monad.Indexed
import Control.Monad.State.Class (MonadState(..))
import Data.Char (toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Word (Word)

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC
import qualified LLVM.General.AST.Global as LLG

import IxState

makeLensesWith (lensRules & lensField .~ (\s -> Just $ "glob" ++ map toUpper (take 1 s) ++ drop 1 s)) ''LL.Global

data BState = BState {
	_instrs :: [LL.Named LL.Instruction],
	_currentBlock :: LL.Name
} deriving Show
makeLenses ''BState

data Terminated' = Terminated' deriving Show

data FState a = FState {
	_basicBlocks :: Map LL.Name (Maybe LL.BasicBlock),
	_basicBlockOrder :: [LL.Name],
	_refCount :: Word,
	_function :: LL.Global,
	_innerState :: a
} deriving Show
makeLenses ''FState

type BasicBlock = FState BState
type Terminated = FState Terminated'

newtype Builder i o a = Builder { unBuilder :: IxState i o a }
	deriving (IxFunctor, IxApplicative, IxPointed, IxMonad, IxMonadState)

deriving instance Prelude.Monad (Builder i i)
deriving instance MonadState i (Builder i i)

type CBuilder s a = Builder s s a
type ABuilder a b = CBuilder (FState a) b 

newtype BasicBlockRef = BasicBlockRef { _unBasicBlockRef :: LL.Name } deriving (Eq, Show)
makeLenses ''BasicBlockRef

newtype ValueRef = ValueRef { _unValueRef :: LL.Operand } deriving (Eq, Show)
makeLenses ''ValueRef

vrFromString :: String -> ValueRef
vrFromString = vrFromName . LL.Name

vrFromName :: LL.Name -> ValueRef
vrFromName = ValueRef . LL.LocalReference

infix 4 .==, %==

(.==) :: IxMonadState m => ASetter s t a b -> b -> m s t ()
l .== b = imodify (l .~ b)   

(%==) :: (Profunctor p, IxMonadState m) => Setting p s t a b -> p a b -> m s t ()
l %== b = imodify (l %~ b)

ixuse :: IxMonadState m => Getting a s a -> m s s a
ixuse l = do
	x <- iget
	return $ x ^. l

initBuilder :: Builder BasicBlock Terminated a -> CBuilder Terminated a
initBuilder fb = do
	entry <- createBasicBlock "entry"
	switchTo entry
	fb

runBuilder :: Builder BasicBlock Terminated a -> String -> LL.Global
runBuilder fb nameStr =
	let initialState = FState M.empty [] 0 LLG.functionDefaults Terminated' in
	let fstate = snd $ runIxState (unBuilder $ initBuilder fb) initialState in
	let f = fstate ^. function in
	f & globName .~ (LL.Name nameStr)
	  & globReturnType .~ LL.IntegerType 32
	  & globBasicBlocks .~ (catMaybes $ M.elems $ fstate ^. basicBlocks)

getAndIncrementCount :: ABuilder a Word
getAndIncrementCount = refCount <<+= 1

getNextUnName :: ABuilder a LL.Name
getNextUnName = do
	c <- getAndIncrementCount
	return $ LL.UnName c

setParameters :: [(LL.Type, String)] -> ABuilder a [ValueRef]
setParameters ps = do
	let params = map (\(t,n) -> LL.Parameter t (LL.Name n) []) ps
	function.globParameters .= (params, False)
	return $ map (vrFromString . snd) ps

createBasicBlock :: String -> ABuilder a BasicBlockRef
createBasicBlock n = do
	c <- getAndIncrementCount
	let name = LL.Name $ n ++ "." ++ show c
	bbs <- ixuse basicBlocks
	if M.member name bbs then error $ "Basic block " ++ n ++ " already exists"
	else do
		basicBlocks %== M.insert name Nothing
		basicBlockOrder %== (name :)
		return $ BasicBlockRef name

switchTo :: BasicBlockRef -> Builder Terminated BasicBlock ()
switchTo (BasicBlockRef n) = do
	bbs <- ixuse basicBlocks
	case M.lookup n bbs of
		Just (Just _) -> error $ "Already switched to " ++ show n
		Just Nothing -> innerState .== BState [] n
		Nothing -> error "wtf?"

appendInstr :: LL.Instruction -> CBuilder BasicBlock ValueRef
appendInstr instr = do
	n <- getNextUnName
	let i = n LL.:= instr
	innerState.instrs %== (i :)
	return $ vrFromName n

add :: ValueRef -> ValueRef -> CBuilder BasicBlock ValueRef
add (ValueRef x) (ValueRef y) = appendInstr $ LL.Add False False x y []

appendTerm :: LL.Terminator -> Builder BasicBlock Terminated ()
appendTerm t = do
	is <- ixuse $ innerState.instrs
	curBlock <- ixuse $ innerState.currentBlock
	n <- getNextUnName
	let bb = LL.BasicBlock curBlock is (n LL.:= t) 
	basicBlocks %== M.insert curBlock (Just bb)
	innerState .== Terminated'

ret :: ValueRef -> Builder BasicBlock Terminated ()
ret (ValueRef x) = appendTerm $ LL.Ret (Just x) []

br :: BasicBlockRef -> Builder BasicBlock Terminated ()
br (BasicBlockRef n) = appendTerm $ LL.Br n [] 

condBr :: ValueRef -> BasicBlockRef -> BasicBlockRef -> Builder BasicBlock Terminated ()
condBr (ValueRef cond) (BasicBlockRef true) (BasicBlockRef false) = appendTerm $ LL.CondBr cond true false []
 
constant :: Integer -> ValueRef
constant = ValueRef . LL.ConstantOperand . LLC.Int 32

c3 :: ValueRef
c3 = constant 3

test :: Builder BasicBlock Terminated ()
test = do
	b <- add c3 c3
	x <- add b b
	ret x
	bl <- createBasicBlock "bl"
	switchTo bl
	y <- add b x
	ret y