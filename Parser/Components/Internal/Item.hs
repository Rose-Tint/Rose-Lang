module Parser.Components.Internal.Item (
    Item(..),
    item,
) where

import Text.Parsec (choice)

import Parser.Components.Identifiers (
    bigIdent,
    prefixIdent,
    )
import Parser.Components.Internal.LangDef (keyword)
import Parser.Data (Parser, Var)


data Item
    = TraitItem {-# UNPACK #-} !Var
    | DataItem {-# UNPACK #-} !Var
    | FuncItem {-# UNPACK #-} !Var
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
