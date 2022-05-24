module AST (
    module AST',
    Import,
    ParseTree(..),
) where

import AST.Expr as AST'
import AST.Literal as AST'
import AST.Pattern as AST'
import AST.Stmt as AST'
import AST.Value as AST'
import Common.Var (Var)
import Text.Pretty


type Import = Var

data ParseTree = ParseTree [Import] [Expr]


instance Pretty ParseTree where
    pretty (ParseTree imps exprs) =
        "\n"`seps`imps|+"\n\n"+|
        unlines (pretty <$> exprs)|+"\n"
    detailed (ParseTree imports exprs) =
        "Imports:\n"
            +|indentCatLnsD imports|+
        "Top-Level Expressions:\n"
            +|indentCatLnsD exprs
