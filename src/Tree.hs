module Tree where

newtype Name = Name String deriving (Eq,Show)

data Namespace = Namespace Name [Decl]
	deriving Show

data Decl
	= Class Name
	| Interface Name
	deriving Show