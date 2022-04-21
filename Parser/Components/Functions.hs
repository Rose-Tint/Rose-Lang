module Parser.Components.Functions (
    funcDecl,
    funcDef,
) where


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
