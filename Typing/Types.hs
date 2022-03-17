module Typing.Types where

import Control.Monad ((<$!>))
import Data.Char (isUpper)
import Data.List (union)
import Data.List.NonEmpty (toList)

import Parser.Data hiding (Type)
import qualified Parser.Data as PD (Type)



data Type
    = Type {
        typeName :: !Variable,
        typeParams :: [Type],
        typeCons' :: [Constraint]
    }
    | Applied {
        typeParams :: ![Type],
        typeCons' :: ![Constraint]
    }
    | Param {
        typeName :: !Variable,
        typeParams :: ![Type],
        typeCons' :: ![Constraint]
    }
    | Delayed {
        typeCons' :: ![Constraint]
    }
    | NoType
    deriving (Show, Eq)



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


-- |Does not throw an error for `@NoType@`s
typeCons :: Type -> [Constraint]
{-# INLINE typeCons #-}
typeCons NoType = []
typeCons t = typeCons' t


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
normalize t = t { typeParams =
    (normalize . addCons (typeCons t)) <$!> (typeParams t) }


isComplete :: Type -> Bool
{-# INLINE isComplete #-}
isComplete NoType = False
isComplete (Delayed _) = False
isComplete t = all isComplete (typeParams t)
