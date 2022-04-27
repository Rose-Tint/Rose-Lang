module Parser.Imports (
    Item(..),
    Import(..),
    moduleImport,
) where

import Common.Item
import Common.Var
import Parser.Data (Visibility)

data Import = Import
    {-# UNPACK #-} !Var -- module name
    {-# UNPACK #-} !Var -- alias
    !Visibility
    (Maybe [(Item)])
