module AST.Stmt (
    Stmt(..),
    Body,
    MatchCase(..),
) where

import AST.Value
import AST.Pattern
import Common.Specifiers
import Common.Var
import Text.Pretty
import Typing.TypeDecl


data Stmt
    = IfElse Value Body Body
    | Loop Stmt Stmt Stmt Body
    | Match Value [MatchCase]
    | NewVar Mutab !Var TypeDecl Value
    | Reassignment !Var Value
    | Return Value
    | ValStmt Value
    | Break
    | Continue
    | NullStmt
    | Compound [Stmt]

data MatchCase = Case Pattern Body

-- TODO: make `newtype`
type Body = [Stmt]


instance Pretty Stmt where
    pretty (IfElse val tb fb) = "if ("+|val|+") {\n"
        +|indentCatLns tb|+"\n    else {\n"
        +|indentCatLns fb|+"}"
    pretty (Loop NullStmt cond NullStmt body) =
        "loop ("+|cond|+") {\n"
        +|indentCatLns body|+"}"
    pretty (Loop init' cond iter body) =
        "loop ("+|init'|+"; "+|cond|+"; "+|iter|+") {\n"
        +|indentCatLns body|+"}"
    pretty (Match val cases) = "match ("+|val|+") {\n"
        +|indentCatLns cases|+"}"
    pretty (NewVar mut name typ val) =
        "let "+|mut|+" "+|name|+|typ|+" = "+|val|+";"
    pretty (Reassignment var val) = var|+" = "+|val|+";"
    pretty Continue = "continue"
    pretty Break = "break"
    pretty (Return val) = "return "+|val|+";"
    pretty (ValStmt val) = val|+";"
    pretty NullStmt = ";"
    pretty (Compound ss) = indentCatLns ss
    detailed (IfElse val tb []) =
        "If: "*|val
            |*|indentLns (indentCatLnsD tb)|+
        "Else:"
    detailed (IfElse val tb fb) =
        "If: "*|val
            |*|indentLns (indentCatLnsD tb)|+
        "Else:\n"
            +|indentLns (indentCatLnsD fb)
    detailed (Loop init' cond iter body) =
        "Loop:\n    Initial: "*|init'|*
        "\n    Condition: "*|cond|*
        "\n    Iteration: "*|iter|*
        "\n    Body:\n"
            +|indentLns (indentCatLnsD body)
    detailed (Match val cases) =
        "Match: "*|val|*"\n"+|
            (indentCatLns $ fmap ("Case: "*|) cases)
    detailed (NewVar mut name typ val) =
        "New Variable:\n    Mutab.: "*|mut|*
        "\n    Type: "*|typ|*
        "\n    Name: "+|name|+
        "\n    Value: "*|val
    detailed (Reassignment var val) =
        "Reassignment:\n    Variable: "*|var|*
        "\n    Value: "*|val
    detailed Continue = "Continue"
    detailed Break = "Break"
    detailed (Return val) = "Return: "*|val
    detailed (ValStmt val) =
        "Expr: "*|val
    detailed NullStmt = "Null Statement"
    detailed (Compound ss) = indentCatLns ss

instance Pretty MatchCase where
    pretty (Case p b) = p|+" \n"+|indentCatLns b

