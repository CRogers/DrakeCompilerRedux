module Gen where

import qualified LLVM.General.AST as LL

import Tree
import Builder

i32 :: LL.Type
i32 = LL.IntegerType 32

genModule :: ClassDeclInfo -> LL.Module
genModule cdi = LL.Module "test" Nothing Nothing [LL.GlobalDefinition $ genClassInfo cdi]

genClassInfo :: ClassDeclInfo -> LL.Global
genClassInfo (ClassDeclInfo (Name n) _ _ cdecl) = runFunctionBuilder (genClassDecl cdecl) n

genClassDecl :: ClassDecl -> FunctionBuilder ()
genClassDecl (ClassProc _ [stmt]) = do
	entry <- createBasicBlockRef "entry"
	buildBasicBlock entry $ genStmt stmt

genStmt :: Stmt -> BasicBlockBuilder ()
genStmt (Return e) = ret (genExpr e)

genExpr :: Expr -> LL.Operand
genExpr (IntLit i) = constant i