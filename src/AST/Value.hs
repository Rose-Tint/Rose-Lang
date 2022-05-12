module AST.Value (

) where

type ValArray = Array Int Value

data Value
    = Literal Literal
    | VarVal Var
    | Application Value [Value]
    | CtorCall Var [Value]
    | Tuple ValArray
    | Array ValArray
    | Lambda [Var] Value
    | IfElseVal Value Value Value
    | MatchVal Value [(Value, Value)]


valPos :: Value -> SrcPos
valPos (IntLit _ p) = p
valPos (FloatLit _ p) = p
valPos (DoubleLit _ p) = p
valPos (CharLit _ p) = p
valPos (StringLit _ p) = p
valPos (VarVal var) = varPos var
valPos (Application _ _) = UnknownPos
valPos (Lambda _ _) = UnknownPos
valPos (CtorCall var _) = varPos var
valPos (Array _) = UnknownPos
valPos (Tuple _) = UnknownPos


instance Pretty Value where
    terse (FloatLit n _) = show n
    terse (VarVal var) = terse var
    terse (Tuple arr) = "("-|","`sepsT`elems arr|-")"
    terse (Array arr) = "["-|","`sepsT`elems arr|-"]"
    terse val = pretty val
    pretty (IntLit n _) = show n
    pretty (FloatLit n _) = show n ++ "f"
    pretty (DoubleLit n _) = show n
    pretty (CharLit c _) = show c
    pretty (StringLit s _) = show s
    pretty (VarVal (Var name _))
        | all isIdChar name = name
        | otherwise = "("+|name|+")"
        where
            isIdChar c = isAlphaNum c || c == '_'
    pretty (Application val args) =
        "("+|val|+" "+|" "`seps`args|+")"
    pretty (CtorCall name args) =
        "("+|name|+" "+|" "`seps`args|+")"
    pretty (Tuple arr) = "("+|", "`seps`elems arr|+")"
    pretty (Array arr) = "[ "+|", "`seps`elems arr|+" ]"
    pretty (Lambda ps stmts) =  " "`seps`ps|+" => {\n"
        +|indentCatLns stmts|+"}"
