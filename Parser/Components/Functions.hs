module Parser.Components.Functions (
    funcDecl,
    funcDef,
) where

import Control.Monad ((<$!>))
import Data.Array (listArray)
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Data.Maybe (catMaybes)
import Text.Parsec
import Data.Text (Text)

import Parser.Data
import Parser.Keywords
import Parser.LangDef
import Parser.Pragmas


default (Int, Double)

{-
func-decl           
func-param-seq      
func-def            
-}


-- = visibility, purity, prefix-ident, type-decl;
funcDecl :: Parser Expr
funcDecl = (do
    vis <- visibility
    pur <- purity
    name <- prefixIdent
    typ <- typeDecl
    return (FuncTypeDecl vis pur name typ)
    ) <?> "function declaration"

-- = pattern, infix-ident, pattern;
infixParamSeq :: Parser (Variable, [Value])
infixParamSeq = do
    lhs <- pattern
    op <- infixIdent
    rhs <- pattern
    return (op, [lhs, rhs])

-- = prefix-ident, {pattern};
prefixParamSeq :: Parser (Variable, [Value])
prefixParamSeq = do
    func <- prefixIdent
    args <- many pattern
    return (func, args)

-- = infix-param-seq | prefix-param-seq
funcParamSeq :: Parser (Variable, [Value])
funcParamSeq = try infixParamSeq <|> prefixParamSeq

-- = func-param-seq, body-assignment;
funcDef :: Parser Expr
funcDef = do
    (name, params) <- funcParamSeq
    bdy <- bodyAssignment
    return (FuncDef name params bdy)
