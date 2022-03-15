module Typing.Types where

import Control.Monad ((<$!>))
import Data.List (union)
import Data.List.NonEmpty (toList)

import Parser.Data hiding (Type)
import qualified Parser.Data as PD (Type)



data Type
    = Type {
        typeName :: Variable,
        typeParams :: [Type],
        typeCons' :: [Constraint]
    }
    | Applied {
        typeParams :: [Type],
        typeCons' :: [Constraint]
    }
    | Param {
        typeName :: Variable,
        typeParams :: [Type],
        typeCons' :: [Constraint]
    }
    | Delayed {
        typeCons' :: [Constraint]
    }
    | NoType
    deriving (Show, Eq)



-- |Creates a `@Type@` from a `Parser.Data.@Type@` 
fromPDType :: PD.Type -> Type
fromPDType (TerminalType tn ps) = 
    let ps' = fromPDType <$!> ps
    in case tn of
        RealType nm -> Type nm ps' []
        TypeParam nm -> Param nm ps' []
fromPDType (NonTermType t1 ts) =
    Applied (fromPDType t1: toList (fromPDType <$!> ts)) []


-- |Does not throw an error for `@NoType@`s
typeCons :: Type -> [Constraint]
typeCons NoType = []
typeCons t = typeCons' t


-- |Adds constraints to the given type
addCons :: [Constraint] -> Type -> Type
addCons _ NoType = NoType
addCons [] t = t
addCons cs t = t { typeCons' = union (typeCons t) cs }


-- |Adds relevant constraints to sub-types
normalize :: Type -> Type
normalize NoType = NoType
normalize t@(Delayed _) = t
normalize t = t { typeParams =
    (normalize . addCons (typeCons t)) <$!> (typeParams t) }


isComplete :: Type -> Bool
isComplete NoType = False
isComplete (Delayed _) = False
isComplete t = all isComplete (typeParams t)
