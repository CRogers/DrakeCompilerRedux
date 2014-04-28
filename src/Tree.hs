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
data Static = Static | NotStatic deriving (Eq,Show)

data Param = Param Name deriving (Eq,Show)

data ClassDeclInfo = ClassDeclInfo Name Visibility Static ClassDecl Expr
	deriving (Eq,Show)

data ClassDecl
	= ClassVar
	| ClassProc [Param]
	deriving (Eq,Show)

data Expr
	= Var String
	deriving (Eq,Show)