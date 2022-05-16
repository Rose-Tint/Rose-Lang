module Common.Var (
    Var(..),
    prim,
) where

import Data.Function (on)
import Data.Ord (comparing)

import Common.SrcPos
import Text.Pretty


data Var = Var {
        varName :: !String,
        _varPos :: SrcPos
    }


instance HasSrcPos Var where
    getPos = _varPos

instance Eq Var where
    (==) = (==) `on` varName

instance Ord Var where
    (<=) = (<=) `on` varName
    (>=) = (>=) `on` varName
    (<) = (<) `on` varName
    (>) = (>) `on` varName
    compare = comparing varName

instance Pretty Var where
    pretty = varName


prim :: String -> Var
prim s = Var s UnknownPos
