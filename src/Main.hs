{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Control.Monad.Error
import LLVM.General.Module
import LLVM.General.Context

import Parser
import Desugar
import Check
import Test
import Gen

failInIO :: forall c. ErrorT String IO c -> IO c
failInIO = either fail return <=< runErrorT

main :: IO ()
main = do
	input <- getLine
	let parsed = testP' classDeclInfo input
	let m = genModule parsed
	output <- withContext (\cxt -> failInIO $ withModuleFromAST cxt m moduleLLVMAssembly)
	putStrLn output
