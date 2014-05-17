{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Control.Monad (liftM)
import Control.Monad.Error
import LLVM.General.Module
import LLVM.General.Context

import Parser
import Desugar
import Check
import Test
import Gen
import Builder
import IxState

failInIO :: forall c. ErrorT String IO c -> IO c
failInIO = either fail return <=< runErrorT

compile :: String -> IO String
compile input = withContext (\cxt -> failInIO $ withModuleFromAST cxt m moduleLLVMAssembly)
	where parsed = testP' classDeclInfo input
	      m = genModule parsed

main :: IO ()
main = (putStrLn <=< compile) =<< getLine
