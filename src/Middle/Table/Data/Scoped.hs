{-# LANGUAGE FlexibleInstances #-}

module Middle.Table.Data.Scoped (
    Scoped(..),
) where

import Common.SrcPos
import Common.Typing
import Front.Parser
import Pretty


data Scoped = Scp {
        scpType :: TypeDecl,
        scpMutab :: Mutab,
        scpPos :: SrcPos
    }
    deriving (Eq)


instance Pretty Scoped where
    pretty = detailed
    detailed (Scp typ mut pos) =
         "| "+|9.>pos|+
        " | "+|6.>mut|+
        " | "+|35.>typ|+
        " |"

-- instance Pretty (String, Scoped) where
--     pretty = detailed
--     detailed (str, scp) = "| "+|15.>str|+" "*|scp
