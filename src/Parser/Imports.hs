module Parser.Imports (
    Module(..),
    Import(..),
) where
import Common.Var
import Parser.Data (Visibility, Expr)
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

instance Pretty Import where
    pretty (Import name vis) =
        "import "+|vis|+" "+|name
