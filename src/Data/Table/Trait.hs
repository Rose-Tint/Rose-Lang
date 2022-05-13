{-# LANGUAGE FlexibleInstances #-}

module Data.Table.Trait (
    Trait(..),
    -- undefFromCons
) where

import Common.SrcPos
import Common.Specifiers
import Common.Var
import Text.Pretty
import Typing.Type


data Trait = Trait {
        -- trtKind :: Kind,
        trtVisib :: Visib,
        trtMeths :: [Var],
        trtImpls :: [Type],
        trtPos :: SrcPos
    }
    -- deriving (Eq)


instance Pretty Trait where
    pretty = detailed
    detailed (Trait vis _meths _impls pos) =
         "| "+|9.>pos|+
        " | "+|6.>vis|+
        -- " | "+|15.>kind|+
        " |"

-- instance Pretty (String, Trait) where
--     pretty = detailed
--     detailed (str, trt) = "| "+|15.>str|+" "*|trt


-- undefFromCons :: Constraint -> Trait
-- undefFromCons cons = Trait {
--     -- trtKind = kindOf cons,
--     trtVisib = Export,
--     trtMeths = [],
--     trtImpls = [],
--     trtPos = UnknownPos
--     }
