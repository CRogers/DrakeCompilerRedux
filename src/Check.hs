module Check where

import Data.List (findIndex)

import Tree

checkNothingAfterReturn :: Block -> Bool
checkNothingAfterReturn stmts =
	maybeToBool $ do
		i <- findIndex f stmts
		return $ (i + 1) == length stmts 
	where f :: Stmt -> Bool
	      f (Return _) = True
	      f _ = False
	      maybeToBool :: Maybe Bool -> Bool
	      maybeToBool (Just b) = b
	      maybeToBool Nothing = True
