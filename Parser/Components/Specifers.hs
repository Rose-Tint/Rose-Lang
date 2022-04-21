module Parser.Components.Specifers (
    purity,
    visibility,
    visibility',
    mutability,
) where

import Text.Parsec

import Parser.Data
import Parser.LangDef (keyword)


purity :: Parser Purity
purity = choice [
        keyword "pure"   >> return Pure,
        keyword "impure" >> return Impure,
        keyword "unsafe" >> return Unsafe
    ] <?> "purity"

visibility' :: Parser Visibility
visibility' = choice [
        keyword "export" >> return Export,
        keyword "intern" >> return Intern
    ] <?> "visibility"

visibility :: Parser Visibility
visibility = option Export visibility'

mutability :: Parser Mutability
mutability = option Pure mutability'

mutability' :: Parser Mutability
mutability' = choice [
        keyword "mut"  >> return Pure,
        keyword "imut" >> return Impure
    ] <?> "mutability"
