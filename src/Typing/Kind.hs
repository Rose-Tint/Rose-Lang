{-# LANGUAGE LambdaCase #-}

module Typing.Kind () where

import Data.Binary

import Text.Pretty


data Kind
    = KStar
    | KApp Kind Kind
    deriving (Eq)


instance Pretty Kind where
    pretty KStar = "*"
    pretty (KApp k1@KApp{} k2) = "("+|k1|+") -> "+|k2
    pretty (KApp k1 k2) = k1|+" -> "+|k2

instance Binary Kind where
    put KStar = putWord8 0
    put (KApp k1 k2) = do
        putWord8 1
        put k1
        put k2
    get = getWord8 >>= \case
        0 -> return KStar
        1 -> KApp <$> get <*> get
        n -> fail $ "Binary.get :: Kind: "++
            "unknown flag ("+|n|+")"
