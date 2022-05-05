module Common.Var (
    Var(..),
    prim
) where

import Data.Function (on)
import Data.Ord (comparing)

import Common.SrcPos
import Pretty


data Var = Var {
        varName :: !String,
        varPos :: SrcPos
    }


instance Eq Var where
    (==) = (==) `on` varName

instance Ord Var where
    (<=) = (<=) `on` varName
    (>=) = (>=) `on` varName
    (<) = (<) `on` varName
    (>) = (>) `on` varName
    compare = comparing varName


prim :: String -> Var
prim s = Var s UnknownPos


instance Pretty Var where
    pretty = varName
