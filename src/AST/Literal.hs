module AST.Literal (
    Literal(..),
) where

import Data.Int (Int64)

import Common.SrcPos
import Text.Pretty


data Literal
    = IntLit {-# UNPACK #-} !Int64 SrcPos
    | FloatLit {-# UNPACK #-} !Float SrcPos
    | DoubleLit {-# UNPACK #-} !Double SrcPos
    | CharLit {-# UNPACK #-} !Char SrcPos
    | StringLit String SrcPos


instance HasSrcPos Literal where
    getPos (IntLit _ p) = p
    getPos (FloatLit _ p) = p
    getPos (DoubleLit _ p) = p
    getPos (CharLit _ p) = p
    getPos (StringLit _ p) = p

instance Pretty Literal where
    terse (FloatLit n _) = show n
    terse l = pretty l
    pretty (IntLit n _) = show n
    pretty (FloatLit n _) = show n ++ "f"
    pretty (DoubleLit n _) = show n
    pretty (CharLit c _) = show c
    pretty (StringLit s _) = show s
