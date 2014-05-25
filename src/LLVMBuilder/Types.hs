{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, ConstraintKinds, TypeFamilies #-}

module LLVMBuilder.Types where

import Control.Lens

import qualified LLVM.General.AST as LL

newtype BasicBlockRef = BasicBlockRef { _unBasicBlockRef :: LL.Name } deriving (Eq, Show)
makeLenses ''BasicBlockRef

newtype ValueRef = ValueRef { _unValueRef :: LL.Operand } deriving (Eq, Show)
makeLenses ''ValueRef

