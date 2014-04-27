{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

module Parser where

import Control.Monad (liftM, void)

import Text.Parsec
import qualified Text.Parsec.Prim as PP 
import qualified Text.Parsec.Token as PT

import Tree

operators = oneOf "+-*/!"

langDef = PT.LanguageDef {
	PT.commentStart    = "/*",
	PT.commentEnd      = "*/",
	PT.commentLine     = "//",
	PT.nestedComments  = False,
	PT.identStart      = letter <|> char '_',
	PT.identLetter     = alphaNum <|> char '_',
	PT.opStart         = operators,
	PT.opLetter        = operators,
	PT.reservedNames   = ["namespace", "class"],
	PT.reservedOpNames = ["=", ";"],
	PT.caseSensitive   = False
}

lexer = PT.makeTokenParser langDef

ident = PT.identifier lexer
reserved = PT.reserved lexer
reservedOp = PT.reservedOp lexer
parens = PT.parens lexer
integer = PT.integer lexer
ws = PT.whiteSpace lexer
symbol = PT.symbol lexer
braces = PT.braces lexer

type Parser a = PP.Stream s m Char => PP.ParsecT s () m a

testP p str = PP.runParser p () "" str

semi :: Parser ()
semi = void $ reservedOp ";"

name :: Parser Name
name = liftM Name ident

namespace :: Parser Namespace
namespace = do
	reserved "namespace"
	n <- name
	cs <- braces $ many decl
	return $ Namespace n cs

decl :: Parser Decl
decl = klass <|> interface

klass :: Parser Decl
klass = do
	reserved "class"
	n <- name
	semi
	return $ Class n

interface :: Parser Decl
interface = do
	reserved "interface"
	n <- name
	semi
	return $ Interface n