module Parser.Imports (
    Module(..),
    Import(..),
) where

import Common.Item
import Common.Var
import Parser.Data (Visibility, Expr)
import Pretty


data Module = Module {-# UNPACK #-} !Var [Import] [Expr]

data Import = Import
    {-# UNPACK #-} !Var -- module name
    !Visibility
    (Maybe [Item])


instance Pretty Module where
    pretty (Module name imps exprs) =
        "module "+|name|+ " where\n"
        +|indentCatLns imps|+"\n\n"
        +|unlines (pretty <$> exprs)|+"\n"

instance Pretty Import where
    pretty (Import name vis Nothing) =
        "import "+|vis|+" "+|name
    pretty (Import name vis (Just items)) =
        "import "+|vis|+" "+|name|+" using {\n"
        +|indentCatLns items|+"\n}"
