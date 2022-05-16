module Typing.Type  (
    Type(..),
    typeToList,
) where

import Common.SrcPos
import Common.Var
import Text.Pretty


infixr 9 :->

data Type
    -- | A defined type (i.e. Int, Maybe a)
    = Type !Var [Type]
    | TypeVar !Var
    -- | Application type (i.e. a -> b, a -> String)
    | Type :-> Type
    | TupleType [Type]
    | ArrayType Type
    deriving (Eq)


-- | Helpful for things like sum-type constructors
typeToList :: Type -> [Type]
typeToList (t1 :-> t2) = (t1:typeToList t2)
typeToList t = [t]


instance HasSrcPos Type where
    getPos (Type name _) = getPos name
    getPos (TypeVar name) = getPos name
    getPos (t1 :-> t2) = t1 <?> t2
    getPos (TupleType types) = foldr (<?>) UnknownPos types
    getPos (ArrayType typ) = getPos typ

instance Pretty Type where
    pretty (Type name []) = pretty name
    pretty (Type name types) = name|+" "+|" "`seps`types
    pretty (TypeVar name) = pretty name
    pretty (t1 :-> t2) = "("+|t1|+" -> "+|t2|+")"
    pretty (TupleType types) = "("+|", "`seps`types|+")"
    pretty (ArrayType typ) = "["+|typ|+"]"
