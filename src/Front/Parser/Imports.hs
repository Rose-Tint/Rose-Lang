module Front.Parser.Imports (
    Module(..),
    Import(..),
) where

import Common.Var
import Front.Parser.Data (Visibility, Expr)
import Pretty


data Module = Module
    [Import]
    [Expr]

data Import = Import
    {-# UNPACK #-} !Var -- module name
    Visibility


instance Pretty Module where
    pretty (Module imps exprs) =
        "\n"`seps`imps|+"\n\n"+|
        unlines (pretty <$> exprs)|+"\n"
    detailed (Module imports exprs) =
        "Imports:\n"
            +|indentCatLnsD imports|+
        "Top-Level Expressions:\n"
            +|indentCatLnsD exprs

instance Pretty Import where
    pretty (Import name vis) =
        "import "+|vis|+" "+|name
    detailed (Import name vis) =
        "("*|vis|*") "*|name
