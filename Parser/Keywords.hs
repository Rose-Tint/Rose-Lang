module Parser.Keywords where

import Text.Parsec

import Parser.Data
import Parser.LangDef



purity :: Parser Purity
{-# INLINABLE purity #-}
purity = choice [
        keyword "pure"   >> return Pure,
        keyword "impure" >> return Impure,
        keyword "unsafe" >> return Unsafe
    ] <?> "purity"


visibility' :: Parser Visibility
{-# INLINABLE visibility' #-}
visibility' = choice [
        keyword "export" >> return Export,
        keyword "intern" >> return Intern
    ] <?> "visibility"


visibility :: Parser Visibility
{-# INLINE visibility #-}
visibility = option Export visibility'


mutability :: Parser Mutability
{-# INLINABLE mutability #-}
mutability = option Pure (choice [
        keyword "mut"  >> return Pure,
        keyword "imut" >> return Impure
    ]) <?> "mutability"
