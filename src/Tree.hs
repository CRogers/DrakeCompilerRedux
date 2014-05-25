{-# LANGUAGE StandaloneDeriving, KindSignatures, DataKinds, GADTs, GeneralizedNewtypeDeriving #-}

module Tree where

import Data.Monoid (Monoid)

newtype Name = Name String deriving (Show,Monoid)
newtype VarName (a :: Type) = VarName String deriving (Show,Monoid)

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

data Block a = Block [Stmt] (Maybe (ReturnStmt a))
deriving instance Show (Block a)

data ClassDecl where
	ClassVar :: Expr a -> ClassDecl
	ClassProc :: [Param] -> Block a -> ClassDecl
deriving instance Show ClassDecl

data Stmt where
	RawExpr :: Expr a -> Stmt
	Assign :: VarName a -> Expr a -> Stmt
	If :: Expr b -> Block a -> Block a -> Stmt
deriving instance Show Stmt

data ReturnStmt (a :: Type) = Return (Expr a)
deriving instance Show (ReturnStmt a)

data Type = Int32 | Boolean
	deriving (Show)

data Expr (a :: Type) where
	Var :: VarName a -> Expr a
	IntLit :: Integer -> Expr Int32
	BoolLit :: Bool -> Expr Boolean
deriving instance Show (Expr a)