module Gen where

import qualified LLVM.General.AST as LL
import qualified LLVM.General.AST.Constant as LLC

import Tree
import Builder

i32 :: LL.Type
i32 = LL.IntegerType 32

boolToI1 :: Bool -> LL.Operand
boolToI1 cond = LL.ConstantOperand $ LLC.Int 1 $ if cond then 1 else 0

genModule :: ClassDeclInfo -> LL.Module
genModule cdi = LL.Module "test" Nothing Nothing [LL.GlobalDefinition $ genClassInfo cdi]

genClassInfo :: ClassDeclInfo -> LL.Global
genClassInfo (ClassDeclInfo (Name n) _ _ cdecl) = runFunctionBuilder (genClassDecl cdecl) n

genClassDecl :: ClassDecl -> FunctionBuilder ()
genClassDecl (ClassProc ps stmts) = do
	setParameters $ zip (repeat i32) (map (\(Param (Name n)) -> n) ps)
	genBlock stmts

genBlock :: Block -> FunctionBuilder ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> FunctionBuilder ()
genStmt (RawExpr e) = addToCurrent $ genExpr e
genStmt (Return e) = addToCurrent $ ret =<< genExpr e
genStmt (If cond then_ else_) = do
	[thenBB, elseBB, afterBB] <- mapM createBasicBlockRef ["then", "else", "after"]
	
	addToCurrent $ do
		cond' <- genExpr cond
		condBr cond' thenBB elseBB
	
	switchTo thenBB
	genBlock then_
	addToCurrent $ br afterBB

	switchTo elseBB
	genBlock else_
	addToCurrent $ br afterBB

	switchTo afterBB


genExpr :: Expr -> BasicBlockBuilder LL.Operand
genExpr (IntLit i) = return $ constant i
genExpr (BoolLit b) = return $ boolToI1 b