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
