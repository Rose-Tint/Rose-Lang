{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types        #-}

module Pretty where

import Control.Monad ((<$!>))
import Data.List (intercalate)
import System.IO (Handle, hPutStrLn)
import Text.Printf(IsChar(..))


default (Int, Double)



class Pretty a where
    pretty :: a -> String
    -- |Like pretty, but a more detailed version. Defaults
    -- to pretty
    detailed :: a -> String
    detailed = pretty
    -- |Like detailed, but even more detailed. Defaults
    -- to detailed
    exhaustive :: a -> String
    exhaustive = detailed
    default pretty :: (Show a) => a -> String
    pretty = show



seps, sepsD, sepsE ::
    (Pretty a) => String -> [a] -> String
seps sep as = intercalate sep (pretty <$!> as)
sepsD sep as = intercalate sep (detailed <$!> as)
sepsE sep as = intercalate sep (exhaustive <$!> as)


prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = putStrLn . pretty


hPrettyPrint :: (Pretty a) => Handle -> a -> IO ()
hPrettyPrint hdl = hPutStrLn hdl . pretty


instance Pretty Char where
    pretty = show

instance (IsChar c) => Pretty [c] where
    pretty = fmap toChar
