{-# LANGUAGE StandaloneDeriving, KindSignatures, DataKinds, GADTs, GeneralizedNewtypeDeriving #-}

module Tree where

import Data.Monoid (Monoid)

newtype Name = Name String deriving (Show,Monoid)
newtype VarName = VarName String deriving (Show,Monoid)

data Namespace = Namespace Name [DeclInfo]
	deriving (Show)

data DeclInfo = DeclInfo Name Decl
	deriving (Show)

data Decl
	= Class [ClassDeclInfo]
	| Interface
	deriving (Show)

data Visibility = Private | Public deriving (Show)
data Static = Static | Instance deriving (Show)

data Param = Param Name deriving (Show)

data ClassDeclInfo = ClassDeclInfo Name Visibility Static ClassDecl
	deriving (Show)

data Block = Block [Stmt] (Maybe ReturnStmt)
deriving instance Show Block

data ClassDecl where
	ClassVar :: Expr -> ClassDecl
	ClassProc :: [Param] -> Block -> ClassDecl
deriving instance Show ClassDecl

data Stmt where
	RawExpr :: Expr -> Stmt
	Assign :: VarName -> Expr -> Stmt
	If :: Expr -> Block -> Block -> Stmt
deriving instance Show Stmt

data ReturnStmt = Return Expr
deriving instance Show ReturnStmt


data Expr where
	Var :: VarName -> Expr
	IntLit :: Integer -> Expr
	BoolLit :: Bool -> Expr
deriving instance Show Expr