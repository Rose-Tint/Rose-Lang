module SymbolTable.SymbolMap where

import qualified Data.Map.Strict as Map

import SymbolTable.SymbolData


type SymbolMap = Map.Map Symbol SymbolData



-- lookup a symbol and require it to be fully
-- defined
require :: Symbol -> SymbolMap -> Maybe SymbolData
{-# INLINABLE require #-}
require s m = search s m >>= ifDefined


search :: Symbol -> SymbolMap -> Maybe SymbolData
{-# INLINABLE search #-}
search = Map.lookup


empty :: SymbolMap
{-# INLINE empty #-}
empty = Map.empty


insert :: Symbol -> SymbolData -> SymbolMap -> SymbolMap
{-# INLINE insert #-}
insert = Map.insertWith stitchSD
