module Parser.Components.Specifiers (
    Purity(..),
    Visibility(..),
    Mutability,
    purity,
    visibility,
    mutability,
) where

import Text.Parsec (choice, (<?>))

import Parser.Components.Internal.LangDef (keyword)
import Parser.Data (
    Parser,
    Purity(..),
    Visibility(..),
    Mutability,
    )


purity :: Parser Purity
purity = choice [
        keyword "pure"   >> return Pure,
        keyword "impure" >> return Impure,
        keyword "unsafe" >> return Unsafe
    ] <?> "purity"

visibility :: Parser Visibility
visibility = choice [
        keyword "export" >> return Extern,
        keyword "intern" >> return Intern
    ] <?> "visibility"

mutability :: Parser Mutability
mutability = choice [
        keyword "mut"  >> return Pure,
        keyword "imut" >> return Impure
    ] <?> "mutability"
