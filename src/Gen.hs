{-# LANGUAGE RebindableSyntax, DataKinds, TypeFamilies #-}

module Gen where

import Prelude hiding (Monad(..))
import IxMonadSyntax

import Control.Monad (void)
import Control.Monad.Indexed ((=<<<))

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

import Tree
import LLVMBuilder.Builder

i32 :: LL.Type
i32 = LL.IntegerType 32

boolToI1 :: Bool -> ValueRef
boolToI1 cond = ValueRef $ LL.ConstantOperand $ LLC.Int 1 $ if cond then 1 else 0

genModule :: ClassDeclInfo -> LL.Module
genModule cdi = LL.Module "test" Nothing Nothing [LL.GlobalDefinition $ genClassInfo cdi]

genClassInfo :: ClassDeclInfo -> LL.Global
genClassInfo (ClassDeclInfo (Name n) _ _ cdecl) = runBuilder (genClassDecl cdecl) n

genClassDecl :: ClassDecl -> Builder Setup Terminated ()
genClassDecl (ClassProc ps stmts) = do
	setParameters $ zip (repeat i32) (map (\(Param (Name n)) -> n) ps)
	entry <- createBasicBlock "entry"
	genBlock_ stmts entry (ret $ constant 3)

genBlock :: IsSetupOrTerminated a => Block -> BasicBlockRef -> BasicBlockRef -> Builder a Terminated ()
genBlock stmts entry exit = genBlock_ stmts entry $ br exit

genBlock_ :: IsSetupOrTerminated a => Block -> BasicBlockRef -> Builder BasicBlock Terminated () -> Builder a Terminated ()
genBlock_ (Block stmts rs) entry exit = do
	switchTo entry
	mapM_ genStmt stmts
	case rs of
		Nothing -> exit
		Just (Return e) -> ret =<<< genExpr e

genStmt :: Stmt -> CBuilder BasicBlock ()
genStmt (RawExpr e) = void $ genExpr e
genStmt (If cond then_ else_) = do
	[thenBB, elseBB, afterBB] <- mapM createBasicBlock ["then", "else", "after"]
	
	cond' <- genExpr cond
	condBr cond' thenBB elseBB
	
	genBlock then_ thenBB afterBB
	genBlock else_ elseBB afterBB

	switchTo afterBB


genExpr :: Expr -> CBuilder BasicBlock ValueRef
genExpr (IntLit i) = return $ constant i
genExpr (BoolLit b) = return $ boolToI1 b