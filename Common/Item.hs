module Common.Item (
    Item(..),
    item,
) where

import Text.Parsec (choice)

import Common.Var
import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef (keyword)
import Parser.Data (Parser)
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

item :: Parser Item
item = choice [
        do  keyword "trait"
            name <- bigIdent
            return (TraitItem name),
        do  keyword "data"
            name <- bigIdent
            return (DataItem name),
        do  name <- prefixIdent
            return (FuncItem name)
    ]


instance Pretty Item where
    terse = terse . itemName
    pretty (TraitItem name) = "trait "+|name
    pretty (DataItem name) = "data "+|name
    pretty (FuncItem name) = pretty name
    detailed (TraitItem name) = "trait "+|name
    detailed (DataItem name) = "datatype "+|name
    detailed (FuncItem name) = "function "+|name
