module Common.SrcPos (
    Offset,
    Line,
    Col,
    SrcPos(UnknownPos, SrcPos),
    HasSrcPos(..),
    (<?>),
    newSrcPos,
    posOffset,
    posCol,
    posLine,
) where

import Text.Pretty


type Line = Int

type Col = Int

type Offset = Int

data SrcPos
    = UnknownPos
    | SrcPos {
        srcOffset :: Offset,
        srcLine :: Line,
        srcCol :: Col
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

posCol :: HasSrcPos a => a -> Col
posCol = srcCol . getPos


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
    terse (SrcPos _off ln col) = ln|+","+|col
    pretty UnknownPos = "(unknown)"
    pretty (SrcPos _off ln col) =
        "ln "+|ln|+", col "+|col
    detailed UnknownPos = "(unknown)"
    detailed (SrcPos _off ln col) =
        "line "+|ln|+", column "+|col
