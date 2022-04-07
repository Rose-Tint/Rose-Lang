module Typing.Traits where

import Analyzer.Analyzer
import Typing.Types


data Trait = Trait {
        trtImpls :: [Type],
        trtMeths :: [String]
    }


impls :: Type -> Trait -> Analyzer Bool
{-# INLINE impls #-}
impls typ = return . elem typ . trtImpls
