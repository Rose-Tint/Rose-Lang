module Common.Module (
    Module(..),
    Import(..),
    Item(..),
) where

import AST.Expr
import Common.Var
import Text.Pretty


data Module = Module [Import] [Expr]

data Import = Import Var

data Item
    = TraitItem { itemName :: Var }
    | DataItem { itemName :: Var }
    | FuncItem { itemName :: Var }
    deriving (Eq)


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
    pretty (Import name) =
        "import "+|name
    detailed (Import name) = pretty name

instance Pretty Item where
    terse = terse . itemName
    pretty (TraitItem name) = "trait "+|name
    pretty (DataItem name) = "data "+|name
    pretty (FuncItem name) = pretty name
    detailed (TraitItem name) = "trait "+|name
    detailed (DataItem name) = "datatype "+|name
    detailed (FuncItem name) = "function "+|name
