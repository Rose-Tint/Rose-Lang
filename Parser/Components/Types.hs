module Parser.Components.Types (
    ttype,
    typeDecl,
) where


-- (ignore mutability for now)
-- = [mutability], "[", type, "]"
-- | [mutability], big-ident, {type}
-- | [mutability], small-ident, {type}
-- | [mutability], "(", type, ",", type, { ",", type } ")"
-- | "(", type, { "->", type }, ")";
ttype :: Parser Type
ttype = choice [
        namedType,
        arrayType,
        try tupleType,
        -- try unitType,
        funcType
    ] <?> "type"
    where
        arrayType = Type (Prim "[]") <$> (:[]) <$> brackets ttype
        tupleType = Type (Prim ",") <$> parens (commaSep1 ttype)
        namedType = Type <$> identifier <*> many ttype
        -- unitType = resOper "()" >> TerminalType (Prim "()") []
        funcType = Applied <$> parens (ttype `sepBy1` resOper "->")

-- = big-ident, small-ident, {small-ident};
constraint :: Parser Constraint
constraint = do
    con <- bigIdent
    args <- many1 smallIdent
    return (Constraint con args)

-- = constraint, { ",", constraint }, ":";
ctxDeclSeq :: Parser Context
ctxDeclSeq = commaSep1 constraint <* resOper ":"

-- = "<", [ctx-decl-seq], type, { "->", type }, ">";
typeDecl :: Parser TypeDecl
typeDecl = angles $ do
    ctx <- option [] ctxDeclSeq
    typ <- ttype `sepBy1` resOper "->"
    return (TypeDecl ctx typ)
