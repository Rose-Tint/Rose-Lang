{-# LANGUAGE LambdaCase #-}

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

import Data.Binary

import Text.Pretty


type Line = Int

type Col = Int

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
{-# INLINE (<?>) #-}
a <?> b = case getPos a of
    UnknownPos -> getPos b
    pos -> pos

newSrcPos :: SrcPos
{-# INLINE newSrcPos #-}
newSrcPos = SrcPos 0 0 0

posOffset :: HasSrcPos a => a -> Offset
{-# INLINE posOffset #-}
posOffset = srcOffset . getPos

posLine :: HasSrcPos a => a -> Line
{-# INLINE posLine #-}
posLine = srcLine . getPos

posCol :: HasSrcPos a => a -> Col
{-# INLINE posCol #-}
posCol = srcCol . getPos


instance HasSrcPos SrcPos where
    {-# INLINABLE getPos #-}
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

instance Binary SrcPos where
    put UnknownPos = putWord8 0
    put (SrcPos off ln col) = do
        putWord8 1
        put off
        put ln
        put col

    get = getWord8 >>= \case
        0 -> return UnknownPos
        1 -> SrcPos <$> get <*> get <*> get
            -- off <- get
            -- ln <- get
            -- col <- get
            -- return (SrcPos off ln col)
        n -> fail $ "Binary.get :: SrcPos: " ++
            "unknown flag ("+|n|+")"
        
