{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Middle.Table.VarMap (
    Trie,
    -- Construction
    T.empty, singleton,
    -- Insertion
    insert,
    -- Query
    lookup, findWithDefault,
    -- Deletion/Updating
    delete, adjust,
    -- Combination
    T.union,
    -- Other
    T.keys, T.isEmpty, T.size, isMemberOf,
) where

import Prelude hiding (lookup)

import Common.Var
import Middle.Table.Data
import Middle.Table.Trie hiding (
    singleton,
    insert,
    lookup,
    findWithDefault,
    delete,
    adjust,
    isMemberOf,
    )
import qualified Middle.Table.Trie as T
import Pretty


singleton :: Var -> a -> Trie a
singleton = T.singleton . varName

insert :: Var -> a -> Trie a -> Trie a
insert = T.insert . varName

lookup :: Var -> Trie a -> Maybe a
lookup = T.lookup . varName

findWithDefault :: a -> Var -> Trie a -> a
findWithDefault def = T.findWithDefault def . varName

delete :: Var -> Trie a -> Trie a
delete = T.delete . varName

adjust :: (a -> a) -> Var -> Trie a -> Trie a
adjust f = T.adjust f . varName

isMemberOf :: Var -> Trie a -> Bool
isMemberOf = T.isMemberOf . varName


instance Pretty (Trie Datatype) where
    pretty = detailed
    detailed t = "Datatypes:\n"++
        "+-Symbol#10-+-Position--+-Visib.-+-Kind#12-+\n"
        +|unlineAssocsD t

instance Pretty (Trie Global) where
    pretty = detailed
    detailed t = "Globals:\n"++
        "+-Symbol#10-+-Position--+-Visib.-+-Purity-+-Type#30-+\n"
        +|unlineAssocsD t

instance Pretty [(Trie Scoped)] where
    pretty = detailed
    detailed ts = "Scopeds:\n"++
        "+-Symbol#10-+-Position--+-Mutab.-+-Type#30-+\n"
        +|(unlines $! unlineAssocsD <$> ts)

instance Pretty (Trie Trait) where
    pretty = detailed
    detailed t = "Traits:\n"++
        "+-Symbol#10-+-Position--+-Visib.-+-Kind#12-+\n"
        +|unlineAssocsD t


unlineAssocsD :: Pretty a => Trie a -> String
unlineAssocsD t = unlines strs
    where
        strs = (\(key, dta) -> "| "+|15.>key|+" "*|dta) <$> assocs'
        assocs' = T.assocs t
