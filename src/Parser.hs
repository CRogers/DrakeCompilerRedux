{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

module Parser where

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
	PT.reservedNames   = ["namespace", "class", "private", "public", "static", "var"],
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
semi = PT.semi lexer
commaSep = PT.commaSep lexer

type Parser a = PP.Stream s m Char => PP.ParsecT s () m a

testP p str = case PP.runParser p () "" str of Right x -> x

name :: Parser Name
name = Name <$> ident

(==>) :: String -> a -> Parser a
str ==> a = reserved str >> return a

defaultChoice :: (String, a) -> (String, a) -> Parser a
defaultChoice (ds, da) (os, oa) = option da $ choice [ds ==> da, os ==> oa]

visibility :: Parser Visibility
visibility = defaultChoice ("private", Private) ("public", Public)

static :: Parser Static
static = option Instance ("static" ==> Static)

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
	(n, cv, e) <- classVar <|>  classProc
	return $ ClassDeclInfo n vis st cv e

classVar :: Parser (Name, ClassDecl, Expr)
classVar = do
	reserved "var"
	n <- name
	reservedOp "="
	semi
	return (n, ClassVar, Var "a")

parameterList :: Parser [Param]
parameterList = commaSep (Param <$> name)

classProc :: Parser (Name, ClassDecl, Expr)
classProc = do
	n <- name
	ps <- parens parameterList
	semi
	return (n, ClassProc ps, Var "a")








