{-# LANGUAGE FlexibleInstances #-}

module Middle.Table.Data.Datatype (
    Datatype(..),
    -- undefFromType,
) where

import Common.SrcPos
import Common.Typing
import Common.Var
import Front.Parser
import Pretty


data Datatype = Datatype {
        -- dtKind :: Kind,
        dtVis :: Visib,
        dtCtors :: [Var],
        dtPos :: SrcPos
    }
    -- deriving (Eq)


instance Pretty Datatype where
    pretty = detailed
    detailed (Datatype vis _ctors pos) =
         "| "+|9.>terse pos|+
        " | "+|6.<vis|+
        -- " | "+|15.<kind|+
        " |"

-- instance Pretty (String, Datatype) where
--     pretty = detailed
--     detailed (str, dt) = "| "+|15.>str|+" "*|dt


-- | Initialized a new, undefined `Datatype`.
-- To be used when an undefined type has
-- been encountered.
-- undefFromType :: Type -> Datatype
-- undefFromType typ = Datatype {
--         -- dtKind = kindOf typ,
--         dtVis = Export,
--         dtCtors = [],
--         dtPos = UnknownPos
--     }
