{-# LANGUAGE LambdaCase #-}

module Typing.Type  (
    Type(..),
    typeToList,
    foldTypes,
    renameTypeVars,
) where

import Data.Binary

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

foldTypes :: [Type] -> Type -> Type
foldTypes [] typ = typ
foldTypes (t1:ts) t2 = t1 :-> foldTypes ts t2


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
    pretty (t1@(_ :-> _) :-> t2) = "("+|t1|+") -> "+|t2
    pretty (t1 :-> t2) = t1|+" -> "+|t2
    pretty (TupleType types) = "("+|", "`seps`types|+")"
    pretty (ArrayType typ) = "["+|typ|+"]"


-- not yet right. Needs more info such as vars in scope
renameTypeVars :: Type -> Type
renameTypeVars = snd . go 0
    where
        for :: Int -> [Type] -> (Int, [Type])
        for !i [] = (i, [])
        for !i (t:ts) =
            let (i', t') = go i t
                (i'', ts') = for i' ts
            in (i'', (t':ts'))
        go :: Int -> Type -> (Int, Type)
        go !i (Type name types) =
            let (i', types') = for i types
            in (i', Type name types')
        go !i (TypeVar (Var name pos)) =
            (i + 1, TypeVar (Var (name|+|i) pos))
        go !i (t1 :-> t2) =
            let (i', t1') = go i t1
                (i'', t2') = go i' t2
            in (i'', t1' :-> t2')
        go !i (ArrayType typ) =
            let (i', typ') = go i typ
            in (i', ArrayType typ')
        go !i (TupleType types) =
            let (i', types') = for i types
            in (i', TupleType types')


instance Binary Type where
    put (Type name pars) = do
        putWord8 0
        put name
        putList pars
    -- the actual name of type-vars ultimately
    -- doesnt matter
    put (TypeVar _) = putWord8 1
    put (t1 :-> t2) = do
        putWord8 2
        put t1
        put t2
    put (TupleType types) = do
        putWord8 3
        putList types
    put (ArrayType typ) = do
        putWord8 4
        put typ

    get = getWord8 >>= \case
        0 -> Type <$> get <*> get
        -- arbitrary name ("a")
        1 -> return (TypeVar (prim "a"))
        2 -> (:->) <$> get <*> get
        3 -> TupleType <$> get
        4 -> ArrayType <$> get
        n -> fail $ "Binary.get :: Type: "++
            "unknown flag ("+|n|+")"
