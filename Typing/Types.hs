module Typing.Types where

import Control.Monad ((<$!>))
import Data.Char (isUpper)
import Data.List (union)
import Data.List.NonEmpty (toList)

import Color
import Parser.Data hiding (Type)
import qualified Parser.Data as PD (Type)
import Pretty


default (Int, Double)



data Type
    = Type {
        typeName' :: !Variable,
        typeParams' :: [Type],
        typeCons' :: [Constraint]
    }
    | Applied {
        typeParams' :: ![Type],
        typeCons' :: ![Constraint]
    }
    | Param {
        typeName' :: !Variable,
        typeParams' :: ![Type],
        typeCons' :: ![Constraint]
    }
    | Delayed {
        typeCons' :: ![Constraint]
    }
    | NoType
    deriving (Show)



infixl 7 <~>
(<~>) :: Type -> Type -> Type
t1 <~> t2 = if t2 == NoType then t1 else case t1 of
    Type _ _ _ -> if t1 == t2 then t2 else NoType
    Applied ts cs ->
        if ts == typeParams t2 then
            addCons cs t2
        else
            NoType
    Param _ ps cs ->
        if length ps == length (typeParams t2) then
            addCons cs t2
        else
            NoType
    Delayed cs -> addCons cs t2
    NoType -> t2



-- |Creates a `@Type@` from a `Parser.Data.@Type@`
fromPDType :: PD.Type -> Type
fromPDType (TerminalType nm ps) =
    let ps' = fromPDType <$!> ps
    in if isUpper (head $! varName nm) then
        Type nm ps' []
    else
        Param nm ps' []
fromPDType (NonTermType t1 ts) =
    Applied (fromPDType t1: toList (fromPDType <$!> ts)) []


fromPDTypes :: [PD.Type] -> Type
fromPDTypes [] = NoType
fromPDTypes [t] = fromPDType t
fromPDTypes ts = Applied (fromPDType <$!> ts) []


-- |Does not throw an error for `@NoType@`s
typeCons :: Type -> [Constraint]
{-# INLINE typeCons #-}
typeCons NoType = []
typeCons t = typeCons' t


typeName :: Type -> Variable
typeName NoType = Prim "NOTYPE(name)"
typeName (Delayed _) = Prim "*"
typeName (Applied _ _) = Prim "**"
typeName t = typeName' t


typeParams :: Type -> [Type]
typeParams NoType = []
typeParams (Delayed _) = []
typeParams t = typeParams' t


-- |Adds constraints to the given type
addCons :: [Constraint] -> Type -> Type
{-# INLINE addCons #-}
addCons _ NoType = NoType
addCons [] t = t
addCons cs t = t { typeCons' = union (typeCons t) cs }


-- |Adds relevant constraints to sub-types
normalize :: Type -> Type
{-# INLINE normalize #-}
normalize NoType = NoType
normalize t@(Delayed _) = t
normalize t = t { typeParams' =
    (normalize . addCons (typeCons t)) <$!> (typeParams t) }


isComplete :: Type -> Bool
{-# INLINE isComplete #-}
isComplete NoType = False
isComplete (Delayed _) = False
isComplete t = all isComplete (typeParams t)



instance Eq Type where
    NoType == _ = False
    _ == NoType = False
    Delayed _ == _ = True
    _ == Delayed _ = True
    t1 == t2 =
        typeName t1 == typeName t2 &&
        typeParams t1 == typeParams t2 &&
        typeCons t1 == typeCons t2


instance Pretty Type where
    pretty (Applied ts _) = printf
        "(%s)" (", " `seps` ts)
    pretty NoType = "NOTYPE"
    pretty t = printf
        "%s%s%s"
        (pretty (typeName t))
        (if null (typeParams t) then "" else " ")
        (" " `seps` typeParams t)
    detailed t@(Applied _ []) = pretty t
    detailed NoType = "NOTYPE"
    detailed t =
        if null (typeCons t) then
            pretty t
        else
            printf "{ %s } %s"
                (", " `sepsD` typeCons t)
                (pretty t)
