{-# LANGUAGE LambdaCase #-}

module Common.Var (
    Var(..),
    prim,
    isSmall,
    isLarge,
) where

import Data.Binary
import Data.Char
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

isSmall :: Var -> Bool
isSmall = not . isLarge

isLarge :: Var -> Bool
isLarge (Var "" _) = False
isLarge (Var (c:_) _) = isUpper c


instance Binary Var where
    put (Var name pos) = do
        put name
        put pos
    get = Var <$> get <*> get
