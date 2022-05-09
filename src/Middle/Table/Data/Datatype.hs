{-# LANGUAGE FlexibleInstances #-}

module Middle.Table.Datatype (
    Datatype(..),
) where

import Common.SrcPos
import Common.Typing.Type
import Common.Var
import Middle.Trie
import Pretty


data Datatype = Datatype {
        dtKind :: Kind,
        dtVis :: Visibility,
        dtCtors :: [Var]
        dtPos :: SrcPos
    }
    deriving (Eq)


instance Pretty Datatype where
    pretty (Datatype kind vis _ctors pos) =
        "Datatype ("+|vis|+"): (kind ("+|kind|+"))"
    detailed (Datatype kind vis _ctors pos) =
         "| "+|9.>terse pos|+
        " | "+|6.<vis|+
        " | "+|15.<kind|+
        " |"

instance Pretty (String, Datatype) where
    pretty = detailed
    detailed (str, dt) = "| "+|15.>str|+" "*|dt


-- | Initialized a new, undefined `Datatype`.
-- To be used when an undefined type has
-- been encountered.
undefFromType :: Type -> Datatype
undefFromType typ = Datatype {
        dtKind = kindOf typ,
        dtVis = Export,
        dtCtors = [],
        dtPos = UnknownPos
    }
