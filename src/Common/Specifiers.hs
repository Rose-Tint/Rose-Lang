module Common.Specifiers (
    Purity(..),
) where

import Text.Pretty


data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq)


instance Pretty Purity where
    pretty Pure = "pure"
    pretty Impure = "impure"
    pretty Unsafe = "unsafe"
    detailed = show
