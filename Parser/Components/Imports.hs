module Parser.Components.Imports (
    ImportItem(..),
    ImportModule(..),
    moduleImport,
) where

import Text.Parsec
import Data.Text (Text)

import Parser.Data
import Parser.LangDef (commaSepEnd, keyword)
import Parser.Components.Identifiers (prefixIdent, bigIdent)
import Parser.Components.Specifers (visibility)


default (Int, Double)


data ImportItem
    = ImpTrait { impName :: Variable }
    | ImpData { impName :: Variable }
    | ImpFunc { impName :: Variable }

data ModuleImport = ModuleImport {
        miModule :: Module,
        miAlias :: String,
        miVisib :: Visibility,
        miItems :: Maybe [ImportIden]
    }

data Module
    = Module Visibility {-# UNPACK #-} !Var
    | UnknownMod
    deriving (Show, Eq, Ord)


item :: Parser ImportItem
item = choice [
        (do
            keyword "trait"
            name <- bigIdent
            return (ImpTrait name)),
        (do
            keyword "data"
            name <- bigIdent
            return (ImpData name)),
        ImpFunc <$> prefixIdent
    ] <?> "import item"

moduleImport :: Parser ModuleImport
moduleImport = (do
    keyword "import"
    vis <- visibility
    name <- bigIdent
    alias <- option name (keyword "as" >> bigIdent)
    items <- commaSepEnd item
    return (ModuleImport name, alias, items)
    ) <?> "import statement"
