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


funcDecl :: Parser Expr
funcDecl = (do
    vis <- visibility
    pur <- purity
    name <- prefixIdent
    typ <- typeDecl
    return (FuncTypeDecl vis pur name typ)
    ) <?> "function declaration"

infixParamSeq :: Parser (Variable, [Value])
infixParamSeq = do
    lhs <- pattern
    op <- infixIdent
    rhs <- pattern
    return (op, [lhs, rhs])

prefixParamSeq :: Parser (Variable, [Value])
prefixParamSeq = do
    func <- prefixIdent
    args <- many pattern
    return (func, args)

paramSeq :: Parser (Variable, [Value])
paramSeq = try infixParamSeq <|> prefixParamSeq

funcDef :: Parser Expr
funcDef = do
    (name, params) <- paramSeq
    bdy <- bodyAssignment
    return (FuncDef name params bdy)
