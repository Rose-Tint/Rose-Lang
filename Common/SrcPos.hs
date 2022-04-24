module Common.SrcPos (
    Line,
    Col,
    SrcPos(..),
    fromParsecPos,
    newModulePos,
) where

import Data.Int (Int8)
import qualified Text.Parsec.Pos as P

import Pretty


type Line = Int

type Col = Int8

data SrcPos
    = UnknownPos
    | SrcPos {
        srcModule :: String,
        srcStartLn :: {-# UNPACK #-} !Line,
        srcEndLn :: {-# UNPACK #-} !Line,
        srcStartCol :: {-# UNPACK #-} !Col,
        srcEndCol :: {-# UNPACK #-} !Col
    }
    deriving (Eq, Ord)


-- |Creates a `SrcPos` from 2
-- `Text.Parsec.Pos.SourcePos`s, the first giving the
-- start-position, and the second giving the end
-- position. Ignores the second argument's name.
fromParsecPos :: P.SourcePos -> P.SourcePos -> SrcPos
fromParsecPos start end = SrcPos
    (P.sourceName start)
    (fromIntegral (P.sourceLine start))
    (fromIntegral (P.sourceLine end))
    (fromIntegral (P.sourceColumn start))
    (fromIntegral (P.sourceColumn end))

newModulePos :: String -> SrcPos
newModulePos name = SrcPos name 0 0 0 0


instance Pretty SrcPos where
    terse UnknownPos = "?"
    terse (SrcPos name sl el sc ec)
        | sl == el = name|+":"+|sl|+","+|sc|+"-"+|ec
        | otherwise = name|+":"+|sl|+"-"+|el|+","+|sc|+"-"+|ec
    pretty UnknownPos = "(unknown)"
    pretty pos = terse pos
    detailed UnknownPos = "(unknown)"
    detailed (SrcPos name sl el sc ec)
        | sl == el =
            "in module "+|name|+
            ", on line "+|sl|+
            ", from col "+|sc|+" to "+|ec
        | otherwise =
            "in module "+|name|+
            ", from line "+|sl|+" to "+|el|+
            ", from col "+|sc|+" to "+|ec
