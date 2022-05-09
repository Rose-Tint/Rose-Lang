module Middle.Table.Data (
    Data(..),
) where

import Common.SrcPos
import Common.Typing.Type
import Common.Var
import Middle.Trie
import Pretty


data Scoped = Scp {
        scpType :: Type,
        scpMutab :: Mutability,
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

instance Pretty (String, Scoped) where
    pretty = detailed
    detailed (str, scp) = "| "+|15.>str|+" "*|scp
