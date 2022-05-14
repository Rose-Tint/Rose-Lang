module AST.Ctor (
    Field(..),
    Ctor(..),
) where

import Common.Specifiers
import Common.SrcPos
import Common.Var
import Text.Pretty
import Typing.Type
import Typing.TypeDecl


data Field = Field !Var Type

data Ctor
    = Record !Var Visib [Field]
    | SumType !Var Visib [Type]


instance HasSrcPos Field where
    getPos (Field name _) = getPos name

instance HasSrcPos Ctor where
    getPos (Record name _ _) = getPos name
    getPos (SumType name _ _) = getPos name

instance Pretty Field where
    pretty (Field name typ) =
        name|+|typeDecl [] typ
    detailed (Field name typ) =
        "Field: "+|name|+|typeDecl [] typ

instance Pretty Ctor where
    pretty (SumType vis name types) =
        vis|+" "+|name|+" "`seps`types
    pretty (Record vis name flds) =
        vis|+" "+|name|+" {\n"+|indentCatLns flds|+"\n}"
    detailed (SumType vis name types) =
        "Constructor (SumType):\n    "*|vis|*
        "\n    Name: "*|name|*
        "\n    Types:\n"+|indentCatLnsD types
    detailed (Record vis name flds) =
        "Constructor (Record):\n    "*|vis|*
        "\n    Name:"*|name|*
        "\n    Fields:\n"*|indentCatLns flds
