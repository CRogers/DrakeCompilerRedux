{-# LANGUAGE RebindableSyntax #-}

module Gen where

import Prelude hiding (Monad(..))
import IxMonadSyntax

import Control.Monad (void)
import Control.Monad.Indexed ((=<<<))

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

import Tree
import Builder

i32 :: LL.Type
i32 = LL.IntegerType 32

boolToI1 :: Bool -> ValueRef
boolToI1 cond = ValueRef $ LL.ConstantOperand $ LLC.Int 1 $ if cond then 1 else 0

genModule :: ClassDeclInfo -> LL.Module
genModule cdi = LL.Module "test" Nothing Nothing [LL.GlobalDefinition $ genClassInfo cdi]

genClassInfo :: ClassDeclInfo -> LL.Global
genClassInfo (ClassDeclInfo (Name n) _ _ cdecl) = runBuilder (genClassDecl cdecl) n

genClassDecl :: ClassDecl -> Builder BasicBlock Terminated ()
genClassDecl (ClassProc ps stmts) = do
	setParameters $ zip (repeat i32) (map (\(Param (Name n)) -> n) ps)
	exit <- createBasicBlock "exit" 
	genBlock stmts exit
	switchTo exit
	ret $ c3

genBlock :: Block -> BasicBlockRef -> Builder BasicBlock Terminated ()
genBlock [] bbr = br bbr
genBlock [Return e] _ = ret =<<< genExpr e
genBlock (s:ss) bbr = genStmt s >> genBlock ss bbr 

genStmt :: Stmt -> CBuilder BasicBlock ()
genStmt (RawExpr e) = void $ genExpr e
genStmt (If cond then_ else_) = do
	[thenBB, elseBB, afterBB] <- mapM createBasicBlock ["then", "else", "after"]
	
	cond' <- genExpr cond
	condBr cond' thenBB elseBB
	
	switchTo thenBB
	genBlock then_ afterBB

	switchTo elseBB
	genBlock else_ afterBB

	switchTo afterBB


genExpr :: Expr -> CBuilder BasicBlock ValueRef
genExpr (IntLit i) = return $ constant i
genExpr (BoolLit b) = return $ boolToI1 b