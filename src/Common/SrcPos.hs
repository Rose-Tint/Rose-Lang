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
    calcWidth,
    getCodeAsRed,
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

-- | returns the source code in its range
getCodeAsRed :: HasSrcPos a => a -> [String] -> String
getCodeAsRed a lines' = case getPos a of
    UnknownPos -> error "unknown position"
    SrcPos sl_ sc_ el_ ec_ ->
        let sc = fromIntegral (min sc_ ec_)
            ec = fromIntegral (max sc_ ec_)
            sl = fromIntegral (max sl_ el_)
            el = fromIntegral (max sl_ el_)
            lns = take (1 + el - sl) (drop sl lines')
        in case lns of
            [] -> error "invalid position"
            (ln:lns') ->
                let (preSC, postSC) = splitAt sc ln
                    (middle, postEC) = case lns' of
                        [] -> splitAt ec postSC
                        tailLns ->
                            let (midLns, lastLn) = splitLast tailLns
                                (preEC, postEC') = splitAt ec (head lastLn)
                                middle' = concatMap ("$r"++) midLns
                            in (postSC++middle'++preEC, postEC')
                in preSC++"$r"+|middle|+"$R"++postEC
    where
        splitLast l = splitAt (length l - 1) l

-- | Calculates the differene between columns
calcWidth :: HasSrcPos a => a -> Int
calcWidth a = case getPos a of
    UnknownPos -> error "invalid position"
    SrcPos _ sc _ ec -> fromIntegral (ec - sc)


instance HasSrcPos SrcPos where
    getPos = id

instance Pretty SrcPos where
    terse UnknownPos = "?"
    terse (SrcPos ln col _ _) = ln|+","+|col
    pretty UnknownPos = "(unknown)"
    pretty (SrcPos ln col _ _) =
        "ln "+|ln|+", col "+|col
    detailed UnknownPos = "(unknown)"
    detailed (SrcPos ln col _ _) =
        "line "+|ln|+", column "+|col
