{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.VarMap (
    module M',
    VarMap,
    singleton, fromList,
    insert,
    lookup, findWithDefault,
    delete, adjust,
    isMemberOf,
) where

import Prelude hiding (lookup)

import Common.Var
import Data.Map as M' (
    assocs,
    empty,
    union,
    elems,
    )
import qualified Data.Map as M


-- type VarMap = Trie
type VarMap = M.Map String


singleton :: Var -> a -> VarMap a
singleton = M.singleton . varName

insert :: Var -> a -> VarMap a -> VarMap a
insert = M.insert . varName

fromList :: [(Var, a)] -> VarMap a
fromList [] = M.empty
fromList ((k,v):kvs) = insert k v (fromList kvs)

lookup :: Var -> VarMap a -> Maybe a
lookup = M.lookup . varName

findWithDefault :: a -> Var -> VarMap a -> a
findWithDefault def = M.findWithDefault def . varName

delete :: Var -> VarMap a -> VarMap a
delete = M.delete . varName

adjust :: (a -> a) -> Var -> VarMap a -> VarMap a
adjust f = M.adjust f . varName

isMemberOf :: Var -> VarMap a -> Bool
isMemberOf = M.member . varName
