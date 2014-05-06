module Gen where

import LLVM.General.AST.Global
import qualified LLVM.General.AST.Constant as LLC
import qualified LLVM.General.AST as LL

import Tree

i32 :: LL.Type
i32 = LL.IntegerType 32

genModule :: ClassDeclInfo -> LL.Module
genModule cdi = LL.Module "test" Nothing Nothing [LL.GlobalDefinition $ genClassInfo cdi]

genClassInfo :: ClassDeclInfo -> LL.Global
genClassInfo (ClassDeclInfo n _ _ cdecl) = genClassDecl n cdecl

genClassDecl :: Name -> ClassDecl -> LL.Global
genClassDecl (Name n) (ClassProc ps [stmt]) =
	functionDefaults {
		name = LL.Name n,
		returnType = i32,
		parameters = (map (\(Param (Name pn)) -> LL.Parameter i32 (LL.Name pn) []) ps, False),
		basicBlocks = [BasicBlock (LL.Name "entry") [] (LL.Do $ genStmt stmt)]
	}

genStmt :: Stmt -> LL.Terminator
genStmt (Return e) = LL.Ret (Just $ LL.ConstantOperand $ genExpr e) []

genExpr :: Expr -> LLC.Constant
genExpr (IntLit i) = LLC.Int 32 i