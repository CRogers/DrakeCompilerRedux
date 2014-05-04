{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tree where

import Data.Monoid (Monoid)

newtype Name = Name String deriving (Eq,Show,Monoid)

data Namespace = Namespace Name [DeclInfo]
	deriving (Eq,Show)

data DeclInfo = DeclInfo Name Decl
	deriving (Eq,Show)

data Decl
	= Class [ClassDeclInfo]
	| Interface
	deriving (Eq,Show)

data Visibility = Private | Public deriving (Eq,Show)
data Static = Static | Instance deriving (Eq,Show)

data Param = Param Name deriving (Eq,Show)

data ClassDeclInfo = ClassDeclInfo Name Visibility Static ClassDecl
	deriving (Eq,Show)

type Block = [Stmt]

data ClassDecl
	= ClassVar Expr
	| ClassProc [Param] Block
	deriving (Eq,Show)

data Stmt
	= RawExpr Expr
	| If Expr Block Block
	| Return Expr
	deriving (Eq,Show)

data Expr
	= Var String
	| IntLit Integer
	deriving (Eq,Show)