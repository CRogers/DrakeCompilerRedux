{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies, StandaloneDeriving, TemplateHaskell, RebindableSyntax, GeneralizedNewtypeDeriving, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module LLVMBuilder.Core where

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
import qualified LLVM.General.AST.Global as LLG

import IxState
import LLVMBuilder.IxLens
import LLVMBuilder.Types

makeLensesWith (lensRules & lensField .~ (\s -> Just $ "glob" ++ map toUpper (take 1 s) ++ drop 1 s)) ''LL.Global

data BasicBlock = BasicBlock {
	_instrs :: [LL.Named LL.Instruction],
	_currentBlock :: LL.Name
} deriving Show
makeLenses ''BasicBlock

data Terminated = Terminated deriving Show
data Setup = Setup deriving Show

data FState a = FState {
	_basicBlocks :: Map LL.Name (Maybe LL.BasicBlock),
	_basicBlockOrder :: [LL.Name],
	_refCount :: Word,
	_function :: LL.Global,
	_innerState :: a
} deriving Show
makeLenses ''FState

newtype Builder' i o a = Builder { unBuilder :: IxState i o a }
	deriving (Functor, IxFunctor, IxApplicative, IxPointed, IxMonad, IxMonadState)

deriving instance Prelude.Monad (Builder' i i)
deriving instance MonadState i (Builder' i i)

type Builder i o a = Builder' (FState i) (FState o) a
type CBuilder s a = Builder s s a

{-
vrFromString :: LLVMType String -> ValueRef t
vrFromString = vrFromName . LL.Name
-}
vrFromName :: SLLVMType t -> LL.Name -> ValueRef t
vrFromName t = (ValueRef t) . LL.LocalReference


runBuilder :: Builder Setup Terminated () -> String -> LL.Global
runBuilder (Builder s) nameStr =
	let initialState = FState M.empty [] 0 LLG.functionDefaults Setup in
	let fstate = snd $ runIxState s initialState in
	let f = fstate ^. function in
	let basicBlockNames = reverse $ fstate ^. basicBlockOrder in
	let bblocks = catMaybes $ catMaybes $ map (flip M.lookup $ fstate ^. basicBlocks) basicBlockNames in
	f & globName .~ (LL.Name nameStr)
	  & globReturnType .~ LL.IntegerType 32
	  & globBasicBlocks .~ bblocks

getAndIncrementCount :: CBuilder a Word
getAndIncrementCount = refCount <<+= 1

getNextUnName :: CBuilder a LL.Name
getNextUnName = do
	c <- getAndIncrementCount
	return $ LL.UnName c
{-
setParameters :: [(LL.Type, String)] -> CBuilder Setup [ValueRef]
setParameters ps = do
	let params = map (\(t,n) -> LL.Parameter t (LL.Name n) []) ps
	function.globParameters .= (params, False)
	return $ map (vrFromString . snd) ps

getParameter :: Int -> CBuilder BasicBlock ValueRef
getParameter i = do
	f <- ixuse function
	let ps = LLG.parameters f
	let (LLG.Parameter _ n _) = fst ps !! i
	return $ vrFromName n
-}
createBasicBlock :: String -> CBuilder a BasicBlockRef
createBasicBlock n = do
	c <- getAndIncrementCount
	let name = LL.Name $ n ++ "." ++ show c
	bbs <- ixuse basicBlocks
	if M.member name bbs then error $ "Basic block " ++ n ++ " already exists"
	else do
		basicBlocks %== M.insert name Nothing
		basicBlockOrder %== (name :)
		return $ BasicBlockRef name

type family SetupOrTerminated a where
	SetupOrTerminated Setup = True
	SetupOrTerminated Terminated = True

type IsSetupOrTerminated a = SetupOrTerminated a ~ True

switchTo :: IsSetupOrTerminated a => BasicBlockRef -> Builder a BasicBlock ()
switchTo (BasicBlockRef n) = do
	bbs <- ixuse basicBlocks
	case M.lookup n bbs of
		Just (Just _) -> error $ "Already switched to " ++ show n
		Just Nothing -> innerState .== BasicBlock [] n
		Nothing -> error "wtf?"

appendInstr :: SLLVMType t -> LL.Instruction -> CBuilder BasicBlock (ValueRef t)
appendInstr t instr = do
	n <- getNextUnName
	let i = n LL.:= instr
	innerState.instrs %== (i :)
	return $ vrFromName t n

appendTerm :: LL.Terminator -> Builder BasicBlock Terminated ()
appendTerm t = do
	is <- ixuse $ innerState.instrs
	curBlock <- ixuse $ innerState.currentBlock
	n <- getNextUnName
	let bb = LL.BasicBlock curBlock (reverse is) (n LL.:= t)
	basicBlocks %== M.insert curBlock (Just bb)
	innerState .== Terminated

