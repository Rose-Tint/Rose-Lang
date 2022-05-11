module Common.Typing.Type  (
    Type(..),
    delayed,
    (<::>),
    normalize,
    -- primitives
    boolType,
    intType,
    floatType,
    doubleType,
    stringType,
    charType,
    arrayType, arrayOf,
    tupleOf,
) where

import Common.Var
import Pretty


data Type
    = Type {-# UNPACK #-} !Var [Type]
    | Param {-# UNPACK #-} !Var [Type]
    | Applied [Type]


instance Eq Type where
    Type nm1 ts1 == Type nm2 ts2 = nm1 == nm2 && allTypesEq ts1 ts2
    Type _ ts1 == Param _ ts2 = allTypesEq ts1 ts2
    Param _ ts1 == Type _ ts2 = allTypesEq ts1 ts2
    Param nm1 ts1 == Param nm2 ts2 = nm1 == nm2 && allTypesEq ts1 ts2
    Param _ [] == Applied _ = True
    Applied _ == Param _ [] = True
    Applied [t1] == t2 = t1 == t2
    t1 == Applied [t2] = t1 == t2
    Applied ts1 == Applied ts2 = allTypesEq ts1 ts2
    _ == _ = False

allTypesEq :: [Type] -> [Type] -> Bool
allTypesEq [] [] = True
allTypesEq (t1:ts1) (t2:ts2) = t1 == t2 && allTypesEq ts1 ts2
allTypesEq _ _ = False


infixl 7 <::>
(<::>) :: Type -> Type -> Maybe Type
Applied [] <::> typ = Just typ
typ <::> Applied [] = Just typ
Applied [t1] <::> t2 = t1 <::> t2
t1 <::> Applied [t2] = t1 <::> t2
Applied ts1 <::> Applied ts2 = case zipTypes ts1 ts2 of
    Nothing -> Nothing
    Just [] -> Nothing
    Just [t] -> Just t
    Just ts -> Just (Applied ts)
Type n1 ts1 <::> Type n2 ts2
    | n1 /= n2 = Nothing
    | otherwise = do
        types <- zipTypes ts1 ts2
        return (Type n1 types)
Param _ ts1 <::> Type nm ts2 = do
    types <- zipTypes ts1 ts2
    return (Type nm types)
Param n1 ts1 <::> Param n2 ts2
    | n1 /= n2 = Nothing
    | otherwise = do
        types <- zipTypes ts1 ts2
        return (Type n1 types)
Type nm ts1 <::> Param _ ts2 = do
        types <- zipTypes ts1 ts2
        return (Type nm types)
typ <::> Param _ [] = Just typ
Param _ [] <::> typ = Just typ
_ <::> _ = Nothing
-- |Zips Types using `(<:>)`. Returns `Nothing` if
-- one list is longer than the other, or `(<:>)`
-- returns `NoType`.
zipTypes :: [Type] -> [Type] -> Maybe [Type]
zipTypes [] [] = Just []
zipTypes [] (_:_) = Nothing
zipTypes (_:_) [] = Nothing
zipTypes (t1:ts1) (t2:ts2) = do
    types <- zipTypes ts1 ts2
    typ <- t1 <::> t2
    return (typ:types)

-- |Turns `Applied` types with only one type-argument
-- into just that type
normalize :: Type -> Type
normalize (Applied [t]) = normalize t
normalize (Applied ts) = Applied (normalize <$> ts)
normalize (Type nm ts) = Type nm (normalize <$> ts)
normalize typ = typ

delayed :: Type
delayed = Param (prim "") []


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
stringType = Type (prim "Array") [charType]

charType :: Type
charType = Type (prim "Char") []

arrayType :: Type
arrayType = Type (prim "[]") [delayed]

arrayOf :: Type -> Type
arrayOf t = Type (prim "[]") [t]

tupleOf :: [Type] -> Type
tupleOf = Type (prim "(,)")


instance Pretty Type where
    -- Tuple
    pretty (Type (Var "(,)" _) types) = "("+|", "`seps`types|+")"
    -- Array
    pretty (Type (Var "[]" _) types) = "["+|", "`seps`types|+"]"
    pretty (Type name []) = pretty name
    pretty (Type name types) = name|+" "+|" "`seps`types
    pretty (Param name []) = pretty name
    pretty (Param name types) = name|+" "+|" "`seps`types
    pretty (Applied types) = "("+|" -> "`seps`types|+")"
