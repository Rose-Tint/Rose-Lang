module Common.SrcPos (
    Offset,
    Line,
    Col,
    SrcPos(UnknownPos, SrcPos),
    HasSrcPos(..),
    (<?>),
    mergePoss,
    newSrcPos,
    posEndCol,
    posEndLine,
    posStartCol,
    posStartLine,
    normPos,
) where

import Data.Int (Int8)

import Text.Pretty


type Line = Int

type Col = Int8

type Offset = Int

data SrcPos
    = UnknownPos
    | SrcPos {
        srcStartLine :: Line,
        srcStartCol :: Col,
        srcEndLine :: Line,
        srcEndCol :: Col
    }
    deriving (Eq, Ord)


class HasSrcPos a where
    getPos :: a -> SrcPos


mergePoss :: (HasSrcPos a, HasSrcPos b) => a -> b -> SrcPos
mergePoss a b = case (getPos a, getPos b) of
    (SrcPos sl1 sc1 el1 ec1, SrcPos sl2 sc2 el2 ec2) -> SrcPos
        (min sl1 sl2)
        (min sc1 sc2)
        (max el1 el2)
        (max ec1 ec2)
    (p1, p2) -> p1 <?> p2

(<?>) :: (HasSrcPos a, HasSrcPos b) => a -> b -> SrcPos
a <?> b = case getPos a of
    UnknownPos -> getPos b
    pos -> pos

normPos :: HasSrcPos a => a -> SrcPos
normPos a = case getPos a of
    UnknownPos -> UnknownPos
    SrcPos sl sc el ec -> SrcPos
        (min sl el)
        (min sc ec)
        (max el sl)
        (max ec sc)

newSrcPos :: SrcPos
newSrcPos = SrcPos 0 0 0 0

posStartLine :: HasSrcPos a => a -> Line
posStartLine = srcStartLine . getPos

posStartCol :: HasSrcPos a => a -> Col
posStartCol = srcStartCol . getPos

posEndLine :: HasSrcPos a => a -> Line
posEndLine = srcEndLine . getPos

posEndCol :: HasSrcPos a => a -> Col
posEndCol = srcEndCol . getPos


instance HasSrcPos SrcPos where
    getPos = id

instance HasSrcPos a => HasSrcPos (Maybe a) where
    getPos Nothing = UnknownPos
    getPos (Just a) = getPos a

instance (HasSrcPos a, HasSrcPos b) => HasSrcPos (Either a b) where
    getPos (Left a) = getPos a
    getPos (Right b) = getPos b

instance HasSrcPos a => HasSrcPos [a] where
    getPos [] = UnknownPos
    getPos (x:xs) = x <?> xs

instance Pretty SrcPos where
    terse UnknownPos = "?"
    terse (SrcPos ln col _ _) = ln|+","+|col
    pretty UnknownPos = "(unknown)"
    pretty (SrcPos ln col _ _) =
        "ln "+|ln|+", col "+|col
    detailed UnknownPos = "(unknown)"
    detailed (SrcPos ln col _ _) =
        "line "+|ln|+", column "+|col
