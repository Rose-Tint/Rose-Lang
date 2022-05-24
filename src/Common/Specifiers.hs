{-# LANGUAGE LambdaCase #-}

module Common.Specifiers (
    Purity(..),
) where

import Data.Binary 

import Text.Pretty


data Purity = Pure | Impure | Unsafe
    deriving (Show, Eq)


instance Pretty Purity where
    pretty Pure = "pure"
    pretty Impure = "impure"
    pretty Unsafe = "unsafe"
    detailed = show

instance Binary Purity where
    put Pure = putWord8 0
    put Impure = putWord8 1
    put Unsafe = putWord8 2
    get = getWord8 >>= \case
        0 -> return Pure
        1 -> return Impure
        2 -> return Unsafe
        n -> fail $ "Binary.get :: Purity: " ++
            "unknown flag ("+|n|+")"
