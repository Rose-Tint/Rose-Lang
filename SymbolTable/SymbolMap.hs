{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SymbolTable.SymbolMap where

import Control.Monad ((<$!>))
import qualified Data.Map.Strict as Map

import Color
import Pretty
import SymbolTable.SymbolData


default (Int, Double)



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


insert :: SymbolData -> SymbolMap -> SymbolMap
{-# INLINE insert #-}
insert dta = Map.insertWith stitchSD (sdVar dta) dta



instance Pretty SymbolMap where
    pretty sm = printf
        "\
\+-Symbol-----+-Type----------------+-Visibility-+-Purity-+\n\
\%s\
\+------------+---------------------+------------+--------+"
        (unlines $! pretty <$!> Map.elems sm)
