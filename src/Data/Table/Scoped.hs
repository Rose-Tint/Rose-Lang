module Data.Table.Scoped (
    Scoped(..),
) where

import Common.SrcPos
import Common.Specifiers
import Text.Pretty


data Scoped = Scp {
        -- scpType :: Type,
        scpMutab :: Mutab,
        _scpPos :: SrcPos
    }


instance Pretty Scoped where
    pretty = detailed
    detailed (Scp mut pos) =
         "| "+|9.>pos|+
        " | "+|6.>mut|+
        -- " | "+|35.>typ|+
        " |"

instance HasSrcPos Scoped where
    getPos = _scpPos

-- instance Pretty (String, Scoped) where
--     pretty = detailed
--     detailed (str, scp) = "| "+|15.>str|+" "*|scp
