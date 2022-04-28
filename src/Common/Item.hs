module Common.Item (
    Item(..),
) where

import Common.Var
import Pretty


data Item
    = TraitItem {
        itemName :: {-# UNPACK #-} !Var
    }
    | DataItem {
        itemName :: {-# UNPACK #-} !Var
    }
    | FuncItem {
        itemName :: {-# UNPACK #-} !Var
    }
    deriving (Eq)


instance Pretty Item where
    terse = terse . itemName
    pretty (TraitItem name) = "trait "+|name
    pretty (DataItem name) = "data "+|name
    pretty (FuncItem name) = pretty name
    detailed (TraitItem name) = "trait "+|name
    detailed (DataItem name) = "datatype "+|name
    detailed (FuncItem name) = "function "+|name
