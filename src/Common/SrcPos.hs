module Common.SrcPos (
    Offset,
    Line,
    Col,
    SrcPos(UnknownPos, SrcPos),
    HasSrcPos(..),
    (<?>),
    posOffset,
    posLine,
    posColumn,
    newSrcPos,
) where

import Data.Int (Int8)

import Text.Pretty


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


class HasSrcPos a where
    getPos :: a -> SrcPos


(<?>) :: (HasSrcPos a, HasSrcPos b) => a -> b -> SrcPos
a <?> b = case getPos a of
    UnknownPos -> getPos b
    pos -> pos

newSrcPos :: SrcPos
newSrcPos = SrcPos 0 0 0

posOffset :: HasSrcPos a => a -> Offset
posOffset = srcOffset . getPos

posLine :: HasSrcPos a => a -> Line
posLine = srcLine . getPos

posColumn :: HasSrcPos a => a -> Col
posColumn = srcCol . getPos


instance HasSrcPos SrcPos where
    getPos = id

instance Pretty SrcPos where
    terse UnknownPos = "?"
    terse (SrcPos _ ln col) = ln|+","+|col
    pretty UnknownPos = "(unknown)"
    pretty (SrcPos _ ln col) =
        "ln "+|ln|+", col "+|col
    detailed UnknownPos = "(unknown)"
    detailed (SrcPos _ ln col) =
        "line "+|ln|+", column "+|col
