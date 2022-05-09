{-# LANGUAGE FlexibleInstances #-}

module Middle.Table.Data (
    Data(..),
) where

import Common.SrcPos
import Common.Typing.Type
import Common.Var
import Middle.Trie
import Pretty


data Global = Global {
        glbType :: Type,
        glbVisib :: Visibility,
        glbPurity :: Maybe Purity,
        glbPos :: SrcPos
    }
    deriving (Eq)


instance Pretty Global where
    pretty = detailed
    detailed (Global typ vis pur pos) =
         "| "+|9.>terse pos|+
        " | "+|6.>vis|+
        " | "+|6.>pur|+
        " | "+|35.>typ|+
        " |"

instance Pretty (String, Global) where
    pretty = detailed
    detailed (str, glb) = "| "+|15.>str|+" "*|glb



undefGlobal :: Global
undefGlobal = Global {
        glbType = Delayed,
        glbVisib = Export,
        glbPurity = Nothing,
        glbPos = UnknownPos
    }
