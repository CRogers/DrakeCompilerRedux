{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

module Parser where

import Control.Monad (void)
import Control.Applicative ((<$>))

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
	PT.reservedNames   = ["namespace", "class", "private", "public", "static"],
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

testP p str = case PP.runParser p () "" str of Right x -> x

semi :: Parser ()
semi = void $ reservedOp ";"

name :: Parser Name
name = Name <$> ident

(==>) :: String -> a -> Parser a
str ==> a = reserved str >> return a

visibility :: Parser Visibility
visibility = option Private $ choice [public, private]
	where public  = "public" ==> Public
	      private = "private" ==> Private

static :: Parser Static
static = option Instance $ choice [stat, inst]
	where stat = "static" ==> Static
	      inst = "instance" ==> Instance

namespace :: Parser Namespace
namespace = do
	reserved "namespace"
	n <- name
	cs <- braces $ many declInfo
	return $ Namespace n cs

declInfo :: Parser DeclInfo
declInfo = do
	(n, decl) <- klass <|> interface
	return $ DeclInfo n decl

klass :: Parser (Name, Decl)
klass = do
	reserved "class"
	n <- name
	cdis <- braces $ many classDeclInfo 
	return (n, Class cdis)

interface :: Parser (Name, Decl)
interface = do
	reserved "interface"
	n <- name
	semi
	return (n, Interface)

classDeclInfo :: Parser ClassDeclInfo
classDeclInfo = do
	vis <- visibility
	st <- static
	n <- name
	semi
	return $ ClassDeclInfo n vis st ClassVar (Var "a")





