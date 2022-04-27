module Common.SrcPos (
    Line,
    Col,
    SrcPos(..),
) where

import Data.Int (Int8)

import Pretty


type Line = Int

type Col = Int8

type Offset = Int

data SrcPos
    = UnknownPos
    | SrcPos {
        srcOffset :: {-# UNPACK #-} !Offset,
        srcLine :: {-# UNPACK #-} !Line,
        srcCol :: {-# UNPACK #-} !Col
    }
    deriving (Eq, Ord)


instance Pretty SrcPos where
    terse UnknownPos = "?"
    terse (SrcPos _ ln col) = ln|+","+|col
    pretty UnknownPos = "(unknown)"
    pretty (SrcPos _ ln col) =
        "ln "+|ln|+", col "+|col
    detailed UnknownPos = "(unknown)"
    detailed (SrcPos _ ln col) =
        "line "+|ln|+", column "+|col
