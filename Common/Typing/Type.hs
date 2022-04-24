module Common.Typing.Type  (
    Type(..),
    (<:>), (<::>),
    normalize,
    -- primitives
    boolType,
    intType,
    floatType,
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
    | Delayed
    | NoType


instance Eq Type where
    NoType == _ = False
    _ == NoType = False
    t1 == t2 = (t1 <:> t2) == NoType


-- |Resolves parameters and simplifies two types
-- as much as possible. Returns `NoType` if they
-- are incompatible, or either are `NoType`.
infixl 7 <:>
(<:>) :: Type -> Type -> Type
typ <:> Delayed = normalize typ
Delayed <:> typ =  normalize typ
Applied [] <:> _ = NoType
_ <:> Applied [] = NoType
Applied [t1] <:> t2 = t1 <:> t2
t1 <:> Applied [t2] = t1 <:> t2
Applied ts1 <:> Applied ts2 =
    case zipTypes ts1 ts2 of
        Nothing -> NoType
        Just ts -> Applied ts
Type n1 ts1 <:> Type n2 ts2
    | n1 /= n2 = NoType
    | otherwise = case zipTypes ts1 ts2 of
        Nothing -> NoType
        Just subs -> Type n1 subs
Param _ ts1 <:> Type nm ts2 =
    case zipTypes ts1 ts2 of
        Nothing -> NoType
        Just subs -> Type nm subs
t@(Type _ _) <:> p@(Param _ _) = p <:> t
_ <:> _ = NoType

-- |Essentially `(<:>)`, but treats `NoType` as
-- `Delayed`. `(<:>)` should be preferred
infixl 7 <::>
(<::>) :: Type -> Type -> Type
typ <::> NoType = normalize typ
NoType <::> typ = normalize typ
t1 <::> t2 = t1 <:> t2

-- |Zips Types using `(<:>)`. Returns `Nothing` if
-- one list is longer than the other, or `(<:>)`
-- returns `NoType`.
zipTypes :: [Type] -> [Type] -> Maybe [Type]
zipTypes [] [] = Just []
zipTypes [] (_:_) = Nothing
zipTypes (_:_) [] = Nothing
zipTypes (t1:ts1) (t2:ts2) = do
    typs <- zipTypes ts1 ts2
    case t1 <:> t2 of
        NoType -> Nothing
        typ -> return (typ:typs)

-- |Turns `Applied` types with only one type-argument
-- into just that type
normalize :: Type -> Type
normalize (Applied []) = NoType
normalize (Applied [t]) = normalize t
normalize (Applied ts) = Applied (normalize <$> ts)
normalize (Type nm ts) = Type nm (normalize <$> ts)
normalize typ = typ


{- PRIMITIVES -}

boolType :: Type
boolType = Type (prim "Boolean") []

intType :: Type
intType = Type (prim "Int") []

floatType :: Type
floatType = Type (prim "Float") []

stringType :: Type
stringType = Type (prim "Array") [charType]

charType :: Type
charType = Type (prim "Char") []

arrayType :: Type
arrayType = Type (prim "[]") [Delayed]

arrayOf :: Type -> Type
arrayOf t = Type (prim "[]") [t]

tupleOf :: [Type] -> Type
tupleOf = Type (prim "(,)")


instance Pretty Type where
    terse Delayed = "*"
    terse NoType = "_"
    terse t = pretty t
    pretty (Type name types) = name|+" "+|" "`seps`types
    pretty (Param name types) = name|+" "+|" "`seps`types
    pretty (Applied types) = "("+|" "`seps`types|+")"
    pretty Delayed = "(DELAYED)"
    pretty NoType = "(NOTYPE)"
