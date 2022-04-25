{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module SymbolTable.SymbolMap (SymbolMap,
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
import Pretty
import qualified SymbolTable.Trie as T
import SymbolTable.SymbolData


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
    terse sm =
        "+-Symbol-----+-Type#17-+\n"
        +|(unlines $! terse <$> T.assocs sm)|+
        "\n+#12-+#22-+"
    pretty sm =
        "+-Symbol#6-+-Type#22-+-Visib.-+\n"
        +|(unlines $! pretty <$> T.assocs sm)|+
        "\n+#17-+#27-+#8-+"
    detailed sm =
        "+-Symbol#11-+-Position---+-Type#27-+-Visib.-+-Purity-+\n"
        +|(unlines $! detailed <$> T.assocs sm)|+
        "\n+#17-+#12-+#32-+#8-+#8-+"
