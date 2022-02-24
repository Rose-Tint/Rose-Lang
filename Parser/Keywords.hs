module Parser.Keywords where

import Text.Parsec

import Parser.Data
import Parser.LangDef



purity :: Parser Purity
purity = lexeme (choice [
        keyword "pure"   >> return Pure,
        keyword "impure" >> return Impure,
        keyword "unsafe" >> return Unsafe
    ]) <?> "purity"


visibility :: Parser Visibility
visibility = lexeme (choice [
        keyword "export" >> return Export,
        keyword "intern" >> return Intern
    ]) <?> "visibility"


mutability :: Parser Mutability
mutability = option Immutable (lexeme (choice [
        keyword "mut"  >> return Mutable,
        keyword "imut" >> return Immutable
    ])) <?> "mutability"
