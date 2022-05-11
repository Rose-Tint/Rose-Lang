module Common.Typing.Type  (
    Type(..),
    typeToList,
    -- primitives
    boolType,
    intType,
    floatType,
    doubleType,
    stringType,
    charType,
    arrayOf,
    tupleOf,
) where

import Common.Var
import Pretty


data Type
    -- | A defined type (i.e. Int, Maybe a)
    = Type !Var [Type]
    | TypeVar !Var
    -- | Application type (i.e. a -> b, a -> String)
    | Type :-> Type
    | TupleType [Type]
    | ArrayType Type


-- | Helpful for things like sum-type constructors
typeToList :: Type -> [Type]
typeToList (t1 :-> t2) = (t1:typeToList t2)
typeToList t = [t]


-- instance Eq Type where
--     Type nm1 ts1 == Type nm2 ts2 = nm1 == nm2 && allTypesEq ts1 ts2
--     Type _ ts1 == TypeVar _ ts2 = allTypesEq ts1 ts2
--     TypeVar _ ts1 == Type _ ts2 = allTypesEq ts1 ts2
--     TypeVar nm1 ts1 == TypeVar nm2 ts2 = nm1 == nm2 && allTypesEq ts1 ts2
--     TypeVar _ [] == Applied _ = True
--     Applied _ == TypeVar _ [] = True
--     Applied [t1] == t2 = t1 == t2
--     t1 == Applied [t2] = t1 == t2
--     Applied ts1 == Applied ts2 = allTypesEq ts1 ts2
--     _ == _ = False

-- allTypesEq :: [Type] -> [Type] -> Bool
-- allTypesEq [] [] = True
-- allTypesEq (t1:ts1) (t2:ts2) = t1 == t2 && allTypesEq ts1 ts2
-- allTypesEq _ _ = False


-- infixl 7 <::>
-- (<::>) :: Type -> Type -> Maybe Type
-- Applied [] <::> typ = Just typ
-- typ <::> Applied [] = Just typ
-- Applied [t1] <::> t2 = t1 <::> t2
-- t1 <::> Applied [t2] = t1 <::> t2
-- Applied ts1 <::> Applied ts2 = case zipTypes ts1 ts2 of
--     Nothing -> Nothing
--     Just [] -> Nothing
--     Just [t] -> Just t
--     Just ts -> Just (Applied ts)
-- Type n1 ts1 <::> Type n2 ts2
--     | n1 /= n2 = Nothing
--     | otherwise = do
--         types <- zipTypes ts1 ts2
--         return (Type n1 types)
-- TypeVar _ ts1 <::> Type nm ts2 = do
--     types <- zipTypes ts1 ts2
--     return (Type nm types)
-- TypeVar n1 ts1 <::> TypeVar n2 ts2
--     | n1 /= n2 = Nothing
--     | otherwise = do
--         types <- zipTypes ts1 ts2
--         return (Type n1 types)
-- Type nm ts1 <::> TypeVar _ ts2 = do
--         types <- zipTypes ts1 ts2
--         return (Type nm types)
-- typ <::> TypeVar _ [] = Just typ
-- TypeVar _ [] <::> typ = Just typ
-- _ <::> _ = Nothing
-- |Zips Types using `(<:>)`. Returns `Nothing` if
-- one list is longer than the other, or `(<:>)`
-- returns `NoType`.
-- zipTypes :: [Type] -> [Type] -> Maybe [Type]
-- zipTypes [] [] = Just []
-- zipTypes [] (_:_) = Nothing
-- zipTypes (_:_) [] = Nothing
-- zipTypes (t1:ts1) (t2:ts2) = do
--     types <- zipTypes ts1 ts2
--     typ <- t1 <::> t2
--     return (typ:types)


{- PRIMITIVES -}

boolType :: Type
boolType = Type (prim "Boolean") []

intType :: Type
intType = Type (prim "Int") []

floatType :: Type
floatType = Type (prim "Float") []

doubleType :: Type
doubleType = Type (prim "Double") []

stringType :: Type
stringType = Type (prim "String") []

charType :: Type
charType = Type (prim "Char") []

arrayOf :: Type -> Type
arrayOf = ArrayType

tupleOf :: [Type] -> Type
tupleOf = TupleType


instance Pretty Type where
    pretty (Type name []) = pretty name
    pretty (Type name types) = name|+" "+|" "`seps`types
    pretty (TypeVar name) = pretty name
    pretty (t1 :-> t2) = "("+|t1|+" -> "+|t2|+")"
    pretty (TupleType types) = "("+|", "`seps`types|+")"
    pretty (ArrayType typ) = "["+|typ|+"]"
