{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Middle.SymbolTable.SymbolMap (SymbolMap,
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

import Common.Var
import qualified Middle.SymbolTable.Trie as T
import Middle.SymbolTable.SymbolData
import Pretty


type SymbolMap = T.Trie SymbolData


singleton :: Symbol -> SymbolData -> SymbolMap
{-# INLINE singleton #-}
singleton = T.singleton . varName

insert :: Symbol -> SymbolData -> SymbolMap -> SymbolMap
{-# INLINE insert #-}
insert sym dta = T.insertWith stitchSD
    (varName sym) (dta { sdPos = Just (varPos sym) })

-- lookup a symbol and require it to be fully
-- defined
require :: Symbol -> SymbolMap -> Maybe SymbolData
{-# INLINE require #-}
require s m = lookup s m >>= ifDefined

lookup :: Symbol -> SymbolMap -> Maybe SymbolData
{-# INLINE lookup #-}
lookup = T.lookup . varName

findWithDefault :: SymbolData -> Symbol -> SymbolMap -> SymbolData
{-# INLINE findWithDefault #-}
findWithDefault def = T.findWithDefault def . varName

delete :: Symbol -> SymbolMap -> SymbolMap
{-# INLINE delete #-}
delete = T.delete . varName

adjust :: (SymbolData -> SymbolData) -> Symbol -> SymbolMap -> SymbolMap
{-# INLINE adjust #-}
adjust f = T.adjust f . varName

isMemberOf :: Symbol -> SymbolMap -> Bool
{-# INLINE isMemberOf #-}
isMemberOf = T.isMemberOf . varName


instance Pretty SymbolMap where
    pretty = detailed
    detailed sm =
        "+-Symbol#10-+-Position--+-Type#32-+-Visib.-+-Purity-+\n"
        +|(unlines $! detailed <$> T.assocs sm)|+
        "+#17-+#11-+#37-+#8-+#8-+"
