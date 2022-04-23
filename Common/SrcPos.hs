module Common.SrcPos (
    Line,
    Col,
    SrcPos(..),
    fromParsecPos,
    newModulePos,
) where

import Data.Int (Int8)
import qualified Text.Parsec.Pos as P


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
    (P.sourceLine start)
    (P.sourceLine end)
    (fromIntegral (P.sourceColumn start))
    (fromIntegral (P.sourceColumn end))

newModulePos :: String -> SrcPos
newModulePos name = SrcPos name 0 0 0 0
