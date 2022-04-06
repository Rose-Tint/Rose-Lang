{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SymbolTable.SymbolMap a (SymbolMap a,
    -- Construction
    T.empty, singleton,
    -- Insertion
    insert,
    -- Query
    require, lookup, findWithDefault,
    -- Deletion/Updating
    delete, adjust,
    -- Combination
    T.union,
    -- Other
    T.keys, T.isEmpty, T.size, isMemberOf,
) where

import Prelude hiding (lookup)

import Control.Monad ((<$!>))

import Color (printf)
import Parser.Data (Variable(..))
import Pretty
import qualified SymbolTable.Trie as T
import SymbolTable.SymbolData


type SymbolMap = T.Trie



singleton :: Symbol -> SymbolData -> SymbolMap a
{-# INLINE singleton #-}
singleton = T.singleton . varName


insert :: Symbol -> SymbolData -> SymbolMap a -> SymbolMap a
{-# INLINE insert #-}
insert sym dta = T.insertWith stitchSD
    (varName sym) (dta { sdPos = Just (varPos sym) })


-- lookup a symbol and require it to be fully
-- defined
require :: Symbol -> SymbolMap a -> Maybe SymbolData
{-# INLINE require #-}
require s m = lookup s m >>= ifDefined


lookup :: Symbol -> SymbolMap a -> Maybe SymbolData
{-# INLINE lookup #-}
lookup = T.lookup . varName


findWithDefault :: SymbolData -> Symbol -> SymbolMap a -> SymbolData
{-# INLINE findWithDefault #-}
findWithDefault def = T.findWithDefault def . varName


delete :: Symbol -> SymbolMap a -> SymbolMap a
{-# INLINE delete #-}
delete = T.delete . varName


adjust :: (SymbolData -> SymbolData) -> Symbol -> SymbolMap a -> SymbolMap a
{-# INLINE adjust #-}
adjust f = T.adjust f . varName


isMemberOf :: Symbol -> SymbolMap a -> Bool
{-# INLINE isMemberOf #-}
isMemberOf = T.isMemberOf . varName



instance Pretty SymbolMap a where
    pretty sm = printf
        "\
\+-Symbol--------+-Type---------------------------+-Visib.-+-Purity-+\n\
\%s\
\+---------------+--------------------------------+--------+--------+"
        (unlines $! pretty <$!> T.assocs sm)
    detailed sm = printf
        "\
\+-Symbol-------------+-Type--------------------------------+-Visib.-+-Purity-+\n\
\%s\
\+--------------------+-------------------------------------+--------+--------+"
        (unlines $! detailed <$!> T.assocs sm)
