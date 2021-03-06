{-# LANGUAGE NoMonomorphismRestriction, Rank2Types, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

module Parser where

import Control.Applicative ((<$>),(<*>),(<*))

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
	PT.reservedNames   = ["namespace", "class", "private", "public", "static", "let", "if", "else", "return", "true", "false"],
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

testP p str = PP.runParser p () "" str
testP' p str = case testP p str of Right x -> x

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
	(n, cv) <- classVar <|> classProc
	return $ ClassDeclInfo n vis st cv

classVar :: Parser (Name, ClassDecl)
classVar = do
	reserved "var"
	n <- name
	reservedOp "="
	e <- expr
	semi
	return (n, ClassVar e)

parameterList :: Parser [Param]
parameterList = commaSep (Param <$> name)

stmt :: Parser Stmt
stmt = do
	s <- choice [rawExpr, assign, if_]
	semi
	return s

rawExpr :: Parser Stmt
rawExpr = RawExpr <$> expr

assign :: Parser Stmt
assign = do
	reserved "let"
	n <- varName
	symbol "="
	e <- expr
	return $ Assign n e

if_ :: Parser Stmt
if_ = do
	reserved "if"
	cond <- expr
	thenPart <- block
	elsePart <- option (Block [] Nothing) else_
	return $ If cond thenPart elsePart
	where else_ = do
		reserved "else"
		block

return_ :: Parser ReturnStmt
return_ = do
	reserved "return"
	Return <$> expr

block :: Parser Block
block = braces $ Block <$> many stmt <*> optionMaybe (return_ <* semi)

classProc :: Parser (Name, ClassDecl)
classProc = do
	n <- name
	ps <- parens parameterList
	stmts <- block
	return (n, ClassProc ps stmts)

expr :: Parser Expr
expr = choice [var, intLit, boolLit]

varName :: Parser VarName
varName = VarName <$> ident

var :: Parser Expr
var = Var <$> varName

intLit :: Parser Expr
intLit = IntLit <$> integer

boolLit :: Parser Expr
boolLit = BoolLit <$> choice ["true" ==> True, "false" ==> False]



