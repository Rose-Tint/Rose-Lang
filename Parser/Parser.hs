{-
TODO:
    Imports are not working (expects extranuous "."),
    some sort of monad to store
        multiple errors
-}

module Parser.Parser (roseParser) where

import Control.Monad ((<$!>))
import Data.Char (isLower)
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Data.Maybe (catMaybes)
import Text.Parsec

import Parser.Data
import Parser.Keywords
import Parser.LangDef


default (Int, Double)



roseParser :: Parser [Expr]
roseParser = do
    wspace
    moduleDecl
    exprs <- manyTill (choice [
            modImport,
            funcDef,
            try funcTypeDecl,
            try dataDef,
            try traitDecl,
            traitImpl
        ])
        eof
    return $! exprs


moduleDecl :: Parser Variable
moduleDecl = (do
    keyword "module"
    name <- moduleName
    keyword "where"
    return $ name)
    <?> "module declaration"


modImport :: Parser Expr
modImport = (do
    keyword "import"
    vis <- option Intern visibility
    name <- moduleName
    return $! ModImport vis name)
    <?> "module import"


arrayLit :: Parser Value
arrayLit = (do
    pos <- getPosition
    arr <- brackets $ commaSep term
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (sourceName pos)
            (sourceLine pos)
            (sourceColumn pos)
            end
    return $! Array (length arr) arr pos')
    <?> "array"


literal :: Parser Value
literal = choice [
        chrLit, strLit,
        intLit, fltLit
    ] <?> "literal"


term :: Parser Value
{-# INLINABLE term #-}
term = choice [
        try literal, arrayLit,
        (\i -> FuncCall i []) <$!> iden,
        parens (ctorCall
            <|> foCall
            <|> term)
    ] <?> "term"


term' :: Parser Value
{-# INLINABLE term' #-}
term' = choice [
        ctorCall,
        foCall,
        term
    ] <?> "term'"


terminalType :: Parser Type
{-# INLINABLE terminalType #-}
terminalType = (do
    name <- iden
    tas <- many ttype
    return $! if isLower (head $ varName name) then
        TerminalType name tas
    else
        TerminalType name tas)
    <?> "terminal type"


nonTermType :: Parser Type
{-# INLINABLE nonTermType #-}
nonTermType = (do
    typs <- fromList <$!> (parens $ commaSep1 ttype)
    case typs of
        (typ :| []) -> return typ
        (t1 :| (t2:ts)) -> return (NonTermType t1 (t2 :| ts)))
    <?> "non-terminal type"


ttype :: Parser Type
{-# INLINABLE ttype #-}
ttype = terminalType <|> nonTermType <|> parens ttype
    <?> "type"


param :: Parser Value
param = choice [
        (\i -> FuncCall i []) <$!> smallIden,
        brackets (ctorCall <|> literal)
    ] <?> "param"


constraint :: Parser Constraint
constraint = (do
    con <- bigIden
    typ <- smallIden
    return $! Constraint con typ)
    <?> "constraint"


typeDecl :: Parser ([Constraint], [Type])
typeDecl = (do
    cons <- option []
        (braces (commaSep constraint) <* comma)
    typs <- commaSep1 ttype
    return $! (cons, typs))
    <?> "type declaration"


foName :: Parser Variable
{-# INLINABLE foName #-}
foName = smallIden <|> parens operator


funcTypeDecl :: Parser Expr
funcTypeDecl = (do
    vis <- visibility
    pur <- purity
    name <- foName
    resOper "=>"
    typDcl <- typeDecl
    let (cons, typs) = typDcl
    semi
    return $! FuncTypeDecl
        pur vis name cons typs)
    <?> "func-type-decl"
 

funcDef :: Parser Expr
funcDef = (do
    name_pars <- choice [
            try (do
                lhs <- param
                op <- operator
                rhs <- param
                return $! (op, [lhs, rhs])
            ),
            (do
                name <- foName
                pars <- many param
                return $! (name, pars)
            )
        ]
    let (name, pars) = name_pars
    bdy <- bodyAssignment
    return $! FuncDef name pars bdy)
    <?> "func-def"


returnE :: Parser Expr
returnE = (do
    keyword "return"
    val <- expr <|> term'
    return $! Return val)
    <?> "return expression"
    where
        expr = resOper "::"
            >> ExprVal <$!> (try match <|> ifElse)


body :: Parser [Expr]
body = braces $ semiSepEnd statement


body' :: Parser [Expr]
body' = ((:[]) <$!> statement)
    <|> braces (semiSepEnd statement)


bodyAssignment :: Parser [Expr]
bodyAssignment = choice [
    body,
    (do resOper ":="
        bdy <- Return <$!> choice [
                try (ExprVal <$!> match),
                try (ExprVal <$!> ifElse),
                term'
            ]
        semi
        return $! [bdy])
    ]


statement :: Parser Expr
statement = choice [
        returnE,
        ifElse,
        loop,
        match,
        newVar,
        try reassign,
        ValueE <$!> funcCall
    ]


reassign :: Parser Expr
reassign = (do
    name <- smallIden
    resOper "="
    val <- term'
    return $! Reassign name val)
    <?> "reassignment"


newVar :: Parser Expr
newVar = (do
    keyword "let"
    mut <- mutability
    name <- smallIden
    typ <- angles ttype
    resOper ":="
    val <- term'
    return $! NewVar mut typ name val)
    <?> "new var"


operCall :: Parser Value
operCall = (do
    lhs <- optionMaybe arg
    op <- operator
    rhs <- optionMaybe arg
    let args = catMaybes [lhs, rhs]
    return $! FuncCall op args)
    <?> "operator call"
    where
        arg = funcCall <|> term


funcCall :: Parser Value
funcCall = (do
    name <- smallIden
    args <- many term
    return $! FuncCall name args)
    <?> "function call"


-- (f)unction or (o)perator (call)
foCall :: Parser Value
{-# INLINABLE foCall #-}
foCall = try operCall <|> funcCall


ctorCall :: Parser Value
ctorCall = (do
    name <- bigIden
    as <- many term
    return $! CtorVal name as)
    <?> "constructor call"


ifElse :: Parser Expr
ifElse = (do
    keyword "if"
    cnd <- term
    tBody <- body'
    fBody <- option [] (keyword "else" >> body')
    return $! IfElse cnd tBody fBody)
    <?> "if-else"


loop :: Parser Expr
loop = (do
    keyword "loop"
    lexeme $ char '('
    ini <- optionMaybe $ statement <* comma
    cnd <- term'
    itr <- optionMaybe $ comma *> statement
    lexeme $ char ')'
    bdy <- body'
    return $! Loop ini cnd itr bdy)
    <?> "loop"


dataDef :: Parser Expr
dataDef = (do
    vis <- visibility
    keyword "data"
    name <- bigIden
    tps <- manyTill smallIden (resOper ":=")
    ctrs <- try (dataCtor vis) `sepBy1` resOper "|="
    return $! DataDef vis name tps ctrs)
    <?> "data-def"


dataCtor :: Visibility -> Parser DataCtor
dataCtor parVis = (do
    vis <- option parVis visibility
    name <- bigIden
    ts <- option [] (resOper "=>" >> commaSep1 ttype)
    return $! DataCtor vis name ts)
    <?> "constructor"


methodDecl ::
    Visibility -> Constraint -> Parser Expr
methodDecl parVis parCon = (do
    pur <- purity
    name <- foName
    resOper "=>"
    typDcl <- typeDecl
    let (cons, typs) = typDcl
    return $! FuncTypeDecl
        pur parVis name (parCon:cons) typs)
    <?> "method declaration"


traitDecl :: Parser Expr
traitDecl = (do
    vis <- visibility
    keyword "trait"
    cons <- option []
        (braces (commaSep1 constraint) <* comma)
    name <- bigIden
    typ <- smallIden
    let thisCon = Constraint name typ
    fns <- braces $ semiSepEnd
        (try $ (methodDecl vis thisCon
            <?> "method declaration"))
    return $! TraitDecl vis cons name typ fns)
    <?> "trait declaration"


traitImpl :: Parser Expr
traitImpl = (do
    keyword "impl"
    cons <- option []
        (braces (commaSep1 constraint) <* comma)
    name <- bigIden
    typ <- optionMaybe ttype
    defs <- braces $ many1 (funcDef <?> "method definition")
    return $! TraitImpl name cons typ defs)
    <?> "trait def"


match :: Parser Expr
match = (do
    keyword "match"
    val <- term
    cases <- braces (many matchCase)
    return $! Pattern val cases)
    <?> "pattern"


matchCase :: Parser (Value, Body)
matchCase = (do
    val <- brackets (ctorCall <|> literal)
    bdy <- bodyAssignment
    return $! (val, bdy))
    <?> "match case"
