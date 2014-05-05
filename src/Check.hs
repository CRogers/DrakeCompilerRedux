module Check where

import Data.List (findIndex)
--import Control.Monad.State.Strict (State, runState)

import Tree

newtype Error = Error { unError :: String } deriving (Eq, Show)

checkNothingAfterReturn :: Block -> Maybe Error
checkNothingAfterReturn stmts =
	toError $ do
		i <- findIndex f stmts
		return $ (i + 1) == length stmts 
	where 
		f :: Stmt -> Bool
		f (Return _) = True
		f _ = False

		toError :: Maybe Bool -> Maybe Error
		toError (Just False) = Just $ Error "Dead code found after return statement" 
		toError _ = Nothing
{-
checkReturnOutOfBlock :: Block -> Bool
checkReturnOutOfBlock stmts =
	where
		g :: Stmt -> Maybe Error
-}