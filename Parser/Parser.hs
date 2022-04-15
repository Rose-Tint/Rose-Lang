module Parser.Parser (roseParser, importsParser) where

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


roseParser :: SourcePos -> Parser [Expr]
roseParser pos = setPosition pos >> manyTill (choice [
        Pragma <$> pragma,
        funcDef,
        try funcTypeDecl,
        try dataDef,
        try traitDecl,
        traitImpl
    ]) eof

importsParser ::
    Parser (String, [ImportModule], Text, SourcePos)
importsParser = do
    wspace
    modName <- moduleDecl
    imports <- many modImport
    rest <- getInput
    pos <- getPosition
    return (varName modName, imports, rest, pos)

moduleDecl :: Parser Variable
moduleDecl = (do
    keyword "module"
    name <- moduleName
    keyword "where"
    return $ name
    ) <?> "module declaration"

modImport :: Parser ImportModule
modImport = (do
    keyword "import"
    vis <- option Intern visibility'
    name <- moduleName
    alias <- option name $ do
        keyword "as"
        moduleName <?> "module alias"
    imps <- optionMaybe $ do
        keyword "using"
        braces (commaSepEnd idenImport)
            <?> "import list"
    return $ Import (varName name) (varName alias) vis imps
    ) <?> "module import"

idenImport :: Parser ImportIden
{-# INLINABLE idenImport #-}
idenImport = try traitImp <|> try dataImp <|> foImp
    where
        {-# INLINE traitImp #-}
        traitImp = (do
            keyword "trait"
            ImportedTrait . varName <$!> bigIden
            ) <?> "trait name"
        {-# INLINE dataImp #-}
        dataImp = (do
            keyword "data"
            ImportedType . varName <$!> bigIden
            ) <?> "datatype name"
        {-# INLINE foImp #-}
        foImp = ImportedFunc . varName <$!> foName

arrayLit :: Parser Value
arrayLit = (do
    pos <- getPosition
    arr <- brackets $ commaSep term
    end <- sourceColumn <$!> getPosition
    let pos' = SourcePos
            (Module Export (Prim $! sourceName pos))
            (sourceLine pos)
            (sourceColumn pos)
            end
    return $ Array (listArray (0, length arr) arr) pos'
    ) <?> "array"

literal :: Parser Value
{-# INLINE literal #-}
literal = choice [
        chrLit, strLit,
        intLit, fltLit
    ] <?> "literal"

term :: Parser Value
{-# INLINABLE term #-}
term = choice [
        try literal, arrayLit,
        (\i -> FuncCall i []) <$!> iden,
        parens (ctorCall <|> foCall <|> term)
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
    return $ TerminalType name tas
    ) <?> "terminal type"

nonTermType :: Parser Type
{-# INLINABLE nonTermType #-}
nonTermType = (do
    typs <- fromList <$!> (parens $ commaSep1 ttype)
    case typs of
        (typ :| []) -> return typ
        (t1 :| (t2:ts)) ->
            return (NonTermType t1 (t2 :| ts))
    ) <?> "non-terminal type"

ttype :: Parser Type
{-# INLINABLE ttype #-}
ttype = choice [
        arrayType,
        terminalType,
        nonTermType,
        parens ttype
    ] <?> "type"
    where
        {-# INLINE arrayType #-}
        arrayType = do
            typ <- brackets ttype
            return $ TerminalType (Prim "Array") [typ]

param :: Parser Value
{-# INLINE param #-}
param = pattern <?> "param"

pattern :: Parser Value
{-# INLINABLE pattern #-}
pattern = choice [
        hole,
        noArgFnCall,
        brackets nonWild
        -- for accepting multiple patterns
        -- brackets (commaSep nonWild)
    ] <?> "pattern"
    where
        {-# INLINE nonWild #-}
        nonWild = ctorPattern <|> literal

noArgFnCall :: Parser Value
{-# INLINE noArgFnCall #-}
noArgFnCall = do
    i <- smallIden
    return (FuncCall i [])

ctorPattern :: Parser Value
{-# INLINE ctorPattern #-}
ctorPattern = (do
    name <- bigIden
    as <- many pattern
    return (CtorVal name as)
    ) <?> "constructor"

constraint :: Parser Constraint
{-# INLINE constraint #-}
constraint = (do
    con <- bigIden
    typ <- smallIden
    return $ Constraint con typ
    ) <?> "constraint"

typeDecl :: Parser ([Constraint], [Type])
typeDecl = (do
    cons <- option []
        (braces (commaSep constraint) <* comma)
    typs <- commaSep1 ttype
    return $ (cons, typs)
    ) <?> "type declaration"

funcTypeDecl :: Parser Expr
funcTypeDecl = (do
    vis <- visibility
    pur <- purity
    name <- foName
    resOper "=>"
    typDcl <- typeDecl
    let (cons, typs) = typDcl
    semi
    return $ FuncTypeDecl pur vis name cons typs
    ) <?> "func-type-decl"

funcDef :: Parser Expr
funcDef = (do
    (name, pars) <- choice [
            try (do
                lhs <- param
                op <- operator
                rhs <- param
                return (op, [lhs, rhs])
            ),
            (do
                name <- foName
                pars <- many param
                return (name, pars)
            )
        ]
    bdy <- bodyAssignment
    return $ FuncDef name pars bdy
    ) <?> "func-def"

returnE :: Parser Expr
{-# INLINABLE returnE #-}
returnE = (do
    keyword "return"
    val <- expr <|> term'
    return $ Return val
    ) <?> "return expression"
    where
        {-# INLINE expr #-}
        expr = resOper "::"
            >> ExprVal <$!> (try match <|> ifElse)

body :: Parser [Expr]
{-# INLINE body #-}
body = braces $ semiSepEnd statement

body' :: Parser [Expr]
{-# INLINE body' #-}
body' = ((:[]) <$!> statement)
    <|> braces (semiSepEnd statement)

bodyAssignment :: Parser [Expr]
{-# INLINE bodyAssignment #-}
bodyAssignment = body <|> (do
    resOper ":="
    bdy <- Return <$!> (
        try (ExprVal <$!> (match <|> ifElse))
        <|> term') <* semi
    return [bdy])

statement :: Parser Expr
statement = choice [
        returnE,
        ifElse,
        loop,
        match,
        newVar,
        try reassign,
        ValueE <$!> funcCall
    ] <?> "statement"

reassign :: Parser Expr
{-# INLINABLE reassign #-}
reassign = (do
    name <- smallIden
    resOper "="
    val <- term'
    return $ Reassign name val
    ) <?> "reassignment"

newVar :: Parser Expr
{-# INLINABLE newVar #-}
newVar = (do
    keyword "let"
    mut <- mutability
    name <- smallIden
    typ <- angles ttype
    resOper ":="
    val <- term'
    return $ NewVar mut typ name val
    ) <?> "new var"

operCall :: Parser Value
{-# INLINABLE operCall #-}
operCall = (do
    lhs <- optionMaybe arg
    op <- operator
    rhs <- optionMaybe arg
    let args = catMaybes [lhs, rhs]
    return $ FuncCall op args
    ) <?> "operator call"
    where
        arg = funcCall <|> term

funcCall :: Parser Value
{-# INLINE funcCall #-}
funcCall = (do
    name <- smallIden
    args <- many term
    return $ FuncCall name args
    ) <?> "function call"

-- (f)unction or (o)perator (call)
foCall :: Parser Value
{-# INLINE foCall #-}
foCall = try operCall <|> funcCall

ctorCall :: Parser Value
{-# INLINABLE ctorCall #-}
ctorCall = (do
    name <- bigIden
    as <- many term
    return $ CtorVal name as
    ) <?> "constructor call"

ifElse :: Parser Expr
{-# INLINABLE ifElse #-}
ifElse = (do
    keyword "if"
    cnd <- term
    tBody <- body'
    fBody <- option [] (keyword "else" >> body')
    return $ IfElse cnd tBody fBody
    ) <?> "if-else"

loop :: Parser Expr
{-# INLINABLE loop #-}
loop = (do
    keyword "loop"
    lexeme $ char '('
    ini <- optionMaybe $ statement <* comma
    cnd <- term'
    itr <- optionMaybe $ comma *> statement
    lexeme $ char ')'
    bdy <- body'
    return $ Loop ini cnd itr bdy
    ) <?> "loop"

dataDef :: Parser Expr
{-# INLINABLE dataDef #-}
dataDef = (do
    vis <- visibility
    keyword "data"
    name <- bigIden
    tps <- manyTill smallIden (resOper ":=")
    ctrs <- try (dataCtor vis) `sepBy1` resOper "|="
    return $ DataDef vis name tps ctrs
    ) <?> "data-def"

dataCtor :: Visibility -> Parser DataCtor
{-# INLINABLE dataCtor #-}
dataCtor parVis = (do
    vis <- option parVis visibility
    name <- bigIden
    ts <- option [] (resOper "=>" >> commaSep1 ttype)
    return $ DataCtor vis name ts
    ) <?> "constructor"

methodDecl :: Visibility -> Constraint -> Parser Expr
methodDecl parVis parCon = (do
    pur <- purity
    name <- foName
    resOper "=>"
    typDcl <- typeDecl
    let (cons, typs) = typDcl
    return $ FuncTypeDecl
        pur parVis name (parCon:cons) typs
    ) <?> "method declaration"

traitDecl :: Parser Expr
traitDecl = (do
    vis <- visibility
    keyword "trait"
    cons <- option []
        (braces (commaSep1 constraint) <* comma)
    name <- bigIden
    typ <- smallIden
    let thisCon = Constraint name typ
    -- TODO: Pragmas should be allowed here
    fns <- braces $ semiSepEnd (try $
        methodDecl vis thisCon)
    return $ TraitDecl vis cons name typ fns
    ) <?> "trait declaration"

traitImpl :: Parser Expr
traitImpl = (do
    keyword "impl"
    cons <- option []
        (braces (commaSep1 constraint) <* comma)
    name <- bigIden
    typ <- optionMaybe ttype
    defs <- braces $ many1
        (funcDef <?> "method definition")
    return $ TraitImpl name cons typ defs
    ) <?> "trait def"

match :: Parser Expr
{-# INLINABLE match #-}
match = (do
    keyword "match"
    val <- term
    cases <- braces (many matchCase)
    return $ Pattern val cases
    ) <?> "pattern"

matchCase :: Parser (Value, Body)
{-# INLINE matchCase #-}
matchCase = (do
    val <- pattern
    bdy <- bodyAssignment
    return $ (val, bdy)
    ) <?> "match case"
