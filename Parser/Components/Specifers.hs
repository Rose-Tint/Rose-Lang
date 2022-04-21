module Parser.Components.Specifers (
    purity,
    visibility,
    visibility',
    mutability,
    mutability',
) where


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

mutability' :: Parser Mutability
mutability' = choice [
        keyword "mut"  >> return Pure,
        keyword "imut" >> return Impure
    ] <?> "mutability"

visibility :: Parser Visibility
visibility = option Export visibility'

mutability :: Parser Mutability
mutability = option Pure mutability'
