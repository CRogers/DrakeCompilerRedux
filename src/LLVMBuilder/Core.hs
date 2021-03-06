{-# LANGUAGE ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, StandaloneDeriving, TemplateHaskell, RebindableSyntax, GeneralizedNewtypeDeriving, TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module LLVMBuilder.Core where

import Prelude hiding (Monad(..))
import qualified Prelude as Prelude
import IxMonadSyntax

import Control.Lens hiding (At)
import Control.Monad.Indexed
import Control.Monad.State.Class (MonadState(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Singletons
import Data.Singletons.Prelude.List (Sing(..), SList)
import Data.Word (Word)

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Global as LLG

import IxState
import LLVMBuilder.IxLens
import LLVMBuilder.Types

data BasicBlock = BasicBlock {
	_instrs :: [LL.Named LL.Instruction],
	_currentBlock :: LL.Name
} deriving Show
makeLenses ''BasicBlock

data Terminated = Terminated deriving Show
data Setup = Setup deriving Show

data FState a (paramTypes :: [LLVMType]) (returnType :: LLVMType) = FState {
	_basicBlocks :: Map LL.Name (Maybe LL.BasicBlock),
	_basicBlockOrder :: [LL.Name],
	_refCount :: Word,
	_parameters :: SList (LLVMTypesToParams paramTypes),
	_returnType :: SLLVMType returnType,
	_innerState :: a
}
makeLenses ''FState

newtype Builder' i o a = Builder { unBuilder :: IxState i o a }
	deriving (Functor, IxFunctor, IxApplicative, IxPointed, IxMonad, IxMonadState)

deriving instance Prelude.Monad (Builder' i i)
deriving instance MonadState i (Builder' i i)

type Builder i ips ir o ops or a = Builder' (FState i ips ir) (FState o ops or) a
type IBuilder i o ps r a = Builder i ps r o ps r a
type CBuilder s ps r a = IBuilder s s ps r a


vrFromString :: SLLVMType t -> String -> VR t
vrFromString t = vrFromName t . LL.Name

vrFromName :: SLLVMType t -> LL.Name -> VR t
vrFromName t = (SValueRef t) . LL.LocalReference

vrFromParam :: SParameter ('Param t) -> VR t
vrFromParam (SParam n t) = vrFromString t n

runBuilder :: Builder Setup '[] 'VoidTy Terminated ops r () -> String -> LL.Global
runBuilder (Builder s) nameStr =
	let initialState = FState M.empty [] 0 SNil SVoidTy Setup in
	let fstate = snd $ runIxState s initialState in
	let basicBlockNames = reverse $ fstate ^. basicBlockOrder in
	let bblocks = catMaybes $ catMaybes $ map (flip M.lookup $ fstate ^. basicBlocks) basicBlockNames in
	let params = fstate ^. parameters in
	let llparams = map (\(ParamTerm n t) -> LL.Parameter (fromLLVM t) (LL.Name n) []) (fromSing params) in
	LLG.functionDefaults {
		LLG.name = LL.Name nameStr,
		LLG.returnType = LL.IntegerType 32,
		LLG.parameters = (llparams, False),
		LLG.basicBlocks = bblocks
	}

getAndIncrementCount :: CBuilder a ps r Word
getAndIncrementCount = refCount <<+= 1

getNextUnName :: CBuilder a ps r LL.Name
getNextUnName = do
	c <- getAndIncrementCount
	return $ LL.UnName c

sListParamToSListValueRef :: SList (t :: [Parameter]) -> SList (ParamsToValueRefs t)
sListParamToSListValueRef SNil = SNil
sListParamToSListValueRef (SCons (SParam n t) ps) = SCons (vrFromString t n) $ sListParamToSListValueRef ps

sAppendName :: SList (t :: [Parameter]) -> CBuilder Setup ps r (SList t)
sAppendName SNil = return SNil
sAppendName (SCons (SParam n t) ps) = do
	c <- getAndIncrementCount
	ps' <- sAppendName ps
	return $ SCons (SParam (n ++ "." ++ show c) t) ps'

slength :: SList (t :: [k]) -> Int
slength SNil = 0
slength (SCons _ xs) = 1 + slength xs 

setParameters :: SList (LLVMTypesToParams ts) -> Builder Setup ips r Setup ts r ()
setParameters ps = do
	renamed <- sAppendName ps
	parameters .== renamed

getParameter :: (At n (LLVMTypesToParams ts) ~ 'Param t) => SNat n -> CBuilder BasicBlock ts r (VR t)
getParameter n = do
	ps <- ixuse parameters
	let p = sAt n ps
	return $ vrFromParam p

setReturnType :: SLLVMType r -> Builder Setup ps q Setup ps r ()
setReturnType r = returnType .== r

createBasicBlock :: String -> CBuilder a ps r BasicBlockRef
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

switchTo :: IsSetupOrTerminated a => BasicBlockRef -> IBuilder a BasicBlock ps r ()
switchTo (BasicBlockRef n) = do
	bbs <- ixuse basicBlocks
	case M.lookup n bbs of
		Just (Just _) -> error $ "Already switched to " ++ show n
		Just Nothing -> innerState .== BasicBlock [] n
		Nothing -> error "wtf?"

appendInstr :: SLLVMType t -> LL.Instruction -> CBuilder BasicBlock ps r (VR t)
appendInstr t instr = do
	n <- getNextUnName
	let i = n LL.:= instr
	innerState.instrs %== (i :)
	return $ vrFromName t n

appendTerm :: LL.Terminator -> IBuilder BasicBlock Terminated ps r ()
appendTerm t = do
	is <- ixuse $ innerState.instrs
	curBlock <- ixuse $ innerState.currentBlock
	n <- getNextUnName
	let bb = LL.BasicBlock curBlock (reverse is) (n LL.:= t)
	basicBlocks %== M.insert curBlock (Just bb)
	innerState .== Terminated

