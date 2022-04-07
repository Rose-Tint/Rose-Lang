{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Warnings where

import Data.Bits ((.|.))
import Data.Word (Word64)

default (Word64)


newtype Warning = W Word64
    deriving (Show, Eq, Ord)

w :: Word64 -> Warning
w = W . (^(2 :: Word64))
w_all = foldr (\(W w2) (W w1) ->
    W (w2 .|. w1)) (W 0) [
            w_name_shadowing,
            w_unused_imports
        ]
w_name_shadowing = w 1
w_unused_imports = w 2
w_unused_exports = w 3

