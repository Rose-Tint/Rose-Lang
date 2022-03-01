{-
TODO:
    Imports are not working (expects extranuous "."),
    USE A MONAD TO AUTOMATICALLY STORE POSITION INFORMATION,
    Use `try` in `thornP` and some sort of monad to store
        multiple errors
-}

module Parser.Parser where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Text.Parsec

import Parser.Data
import Parser.Keywords
import Parser.LangDef



thornP :: Parser [Expr]
thornP = do
    _ <- wspace
    _ <- moduleDecl
    manyTill (choice [
            modImport,
            funcDef,
            try funcTypeDecl,
            try dataDef,
            try traitDecl,
            traitImpl
        ])
        eof


moduleName :: Parser String
moduleName = intercalate "."
    <$> (bigIden `sepBy1` dot)
    <?> "module name"


moduleDecl :: Parser String
moduleDecl = (do
    _ <- keyword "module"
    name <- moduleName
    _ <- keyword "where"
    return $! name)
    <?> "module declaration"


modImport :: Parser Expr
modImport = (do
    _ <- keyword "import"
    vis <- option Intern visibility
    name <- moduleName
    return $! ModImport vis name)
    <?> "module import"


arrayLit :: Parser Value
arrayLit = (do
    arr <- brackets $ commaSep term
    return $! (Array (length arr) arr))
    <?> "array"


term :: Parser Value
term = lexeme (choice [
        chrLit, strLit,
        intLit, fltLit,
        arrayLit,
        VarVal <$> iden,
        parens (ctorCall
            <|> foCallVal
            <|> term)
    ]) <?> "term"


terminalType :: Parser Type
terminalType = (do
    ht <- iden
    tas <- many ttype
    return $! TerminalType ht tas)
    <?> "terminal type"


nonTermType :: Parser Type
nonTermType = lexeme
    (parens (NonTermType <$> commaSep1 ttype))
    <?> "non-terminal type"


ttype :: Parser Type
ttype = terminalType <|> nonTermType
    <?> "type"


param :: Parser Value
param = lexeme (choice [
        VarVal <$> smallIden,
        brackets ctorCall
    ]) <?> "param"


constraint :: Parser (Variable, Variable)
constraint = (do
    con <- bigIden
    typ <- smallIden
    return $! (con, typ))
    <?> "constraint"


typeDecl :: Parser ([Constraint], [Type])
typeDecl = (do
    cons <- option []
        (braces (commaSep constraint) <* comma)
    typs <- commaSep1 ttype
    return $! (cons, typs))
    <?> "type declaration"


foName :: Parser String
foName = smallIden <|> parens operator


funcTypeDecl :: Parser Expr
funcTypeDecl = (do
    vis <- visibility
    pur <- purity
    name <- foName
    _ <- resOper "=>"
    typDcl <- typeDecl
    let (cons, typs) = typDcl
    _ <- semi
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
    bdy <- fnBody
    return $! FuncDef name pars bdy)
    <?> "func-def"
    where
        fnBody = choice [
            body,
            (do _ <- resOper ":="
                bdy <- Return <$> term
                _ <- semi
                return $! [bdy])
            ]


returnE :: Parser Expr
returnE = (do
    _ <- keyword "return"
    val <- term
    return $! Return val)
    <?> "return expression"


body :: Parser [Expr]
body = braces $ semiSepEnd statement


body' :: Parser [Expr]
body' = ((:[]) <$> statement)
    <|> braces (semiSepEnd statement)


statement :: Parser Expr
statement = lexeme (choice [
    try returnE,
    try ifElse,
    try loop,
    try reassign,
    try newVar,
    try match,
    funcCall
    ]) <?> "statement"


reassign :: Parser Expr
reassign = (do
    name <- smallIden
    _ <- resOper "="
    val <- term
    return $! Reassign name val)
    <?> "reassignment"


newVar :: Parser Expr
newVar = (do
    mut <- mutability
    name <- smallIden
    typ <- angles ttype
    _ <- resOper ":="
    val <- term
    return $! NewVar mut typ name val)
    <?> "new var"


operCall :: Parser Expr
operCall = (do
    lhs <- optionMaybe arg
    op <- operator
    rhs <- optionMaybe arg
    let args = catMaybes [lhs, rhs]
    return $! FuncCall op args)
    <?> "operator call"
    where
        -- MAY OR MAY NOT NEED A TRY
        arg = funcCallVal <|> term


funcCall :: Parser Expr
funcCall = (do
    name <- smallIden
    args <- many term
    return $! FuncCall name args)
    <?> "function call"


-- (f)unction or (o)perator (call)
foCall :: Parser Expr
foCall = try operCall <|> funcCall


funcCallVal, operCallVal, foCallVal
    :: Parser Value
funcCallVal = ExprVal <$> funcCall
operCallVal = ExprVal <$> operCall
foCallVal = ExprVal <$> foCall


ctorCall :: Parser Value
ctorCall = (do
    name <- bigIden
    as <- many term
    return $! CtorVal name as)
    <?> "constructor call"


ifElse :: Parser Expr
ifElse = (do
    _ <- keyword "if"
    cnd <- term
    tBody <- body'
    fBody <- option [] (keyword "else" >> body')
    return $! IfElse cnd tBody fBody)
    <?> "if-else"


loop :: Parser Expr
loop = (do
    _ <- keyword "loop"
    _ <- lexeme $ char '('
    ini <- optionMaybe $ statement <* comma
    cnd <- term
    itr <- optionMaybe $ comma *> statement
    _ <- lexeme $ char ')'
    bdy <- body'
    return $! Loop ini cnd itr bdy)
    <?> "loop"


dataDef :: Parser Expr
dataDef = (do
    vis <- visibility
    _ <- keyword "data"
    name <- bigIden
    tps <- manyTill smallIden (resOper ":=")
    ctrs <- dataCtor vis `sepBy1` resOper "|="
    _ <- semi
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
    _ <- resOper "=>"
    typDcl <- typeDecl
    let (cons, typs) = typDcl
    return $! FuncTypeDecl
        pur parVis name (parCon:cons) typs)
    <?> "method declaration"


traitDecl :: Parser Expr
traitDecl = (do
    vis <- visibility
    _ <- keyword "trait"
    cons <- option []
        (braces (commaSep1 constraint) <* comma)
    name <- bigIden
    typ <- smallIden
    let thisCon = (name, typ)
    _ <- keyword "where"
    fns <- semiSepEnd
        (try $ (methodDecl vis thisCon
            <?> "method declaration"))
    return $! TraitDecl vis cons name typ fns)
    <?> "trait declaration"


traitImpl :: Parser Expr
traitImpl = (do
    _ <- keyword "impl"
    name <- bigIden
    typ <- optionMaybe ttype
    _ <- keyword "where"
    defs <- many1 (funcDef <?> "method definition")
    return $! TraitImpl name typ defs)
    <?> "trait def"


match :: Parser Expr
match = (do
    _ <- keyword "match"
    val <- term
    cases <- braces (many matchCase)
    return $! Pattern val cases)
    <?> "pattern"


matchCase :: Parser ([Value], Body)
matchCase = (do
    vals <- brackets (choice [
            try (commaSep nullCtor),
            (:[]) <$> ctorCall,
            commaSep literal
        ])
    bdy <- body
    return $! (vals, bdy))
    <?> "match case"
    where
        nullCtor = (do
            nm <- bigIden
            return $! CtorVal nm [])
            <?> "nullary constructor"
        literal = strLit <|> intLit <|> fltLit
            <?> "literal"
