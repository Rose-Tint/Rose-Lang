{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.VarMap (
    module Data.Trie,
    VarMap,
    -- Construction
    singleton, fromList,
    -- Insertion
    insert,
    -- Query
    lookup, findWithDefault,
    -- Deletion/Updating
    delete, adjust,
    -- Combination
    -- Other
    isMemberOf,
) where

import Prelude hiding (lookup)

import Common.Var
import Data.Table.Datatype
import Data.Table.Global
import Data.Table.Scoped
import Data.Table.Trait
import Data.Trie hiding (
    singleton,
    insert,
    fromList,
    lookup,
    findWithDefault,
    delete,
    adjust,
    isMemberOf,
    )
import qualified Data.Trie as T
import Text.Pretty


type VarMap = Trie


singleton :: Var -> a -> Trie a
singleton = T.singleton . varName

insert :: Var -> a -> Trie a -> Trie a
insert = T.insert . varName

fromList :: [(Var, a)] -> Trie a
fromList [] = empty
fromList ((k,v):kvs) = insert k v (fromList kvs)

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
