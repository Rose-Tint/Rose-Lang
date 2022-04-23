module Parser.Components.Imports (
    Item(..),
    Import(..),
    moduleImport,
) where

import Text.Parsec (
    (<?>),
    option, optionMaybe,
    )

import Common.Item
import Common.Var
import Parser.Components.Identifiers
import Parser.Components.Internal.LangDef (
    keyword,
    braces,
    commaSepEnd,
    )
import Parser.Components.Specifiers
import Parser.Data (
    Parser,
    Visibility,
    )

data Import = Import
    {-# UNPACK #-} !Var -- module name
    {-# UNPACK #-} !Var -- alias
    !Visibility
    (Maybe [(Item)])

{- 
func-import-list = prefix-ident, [ ",", func-import-list ], [","];
ctor-import-list = big-ident, [ ",", ctor-import-list ], [","];
-}

-- (ignore `func-imp-list` and `ctor-imp-list` for now)
-- = "trait", big-ident
-- | "data", big-ident
-- | prefix-ident
-- item :: Parser Item
-- item = choice [
--         do  keyword "trait"
--             name <- bigIdent
--             return (TraitItem name),
--         do  keyword "data"
--             name <- bigIdent
--             return (DataItem name),
--         FuncItem <$> prefixIdent
--     ] <?> "import item"

-- = "import", visibility, big-ident,
--     [ "as", big-ident ],
--     [ "using", "{", {import-item}, "}" ];
moduleImport :: Parser Import
moduleImport = (do
    keyword "import"
    vis <- visibility
    name <- bigIdent <?> "module name"
    alias <- option name $ do
        keyword "as"
        bigIdent <?> "module alias"
    items <- optionMaybe $ do
        keyword "using"
        braces (commaSepEnd item)
    return (Import name alias vis items)
    ) <?> "import statement"
