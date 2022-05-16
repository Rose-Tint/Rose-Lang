{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Typing.Substitution (
    Substable(..),
    Subst,
    nullSubst,
    compose,
    (<|>),
    (~>),
) where

import Data.Foldable (fold, foldl')
import qualified Data.Set as S

import Data.VarMap
import Common.Var
import Typing.Scheme
import Typing.Type


type Subst = VarMap Type


nullSubst :: Subst
nullSubst = empty

compose :: [Subst] -> Subst
compose = fold

infixl 8 <|>
(<|>) :: Subst -> Subst -> Subst
s1 <|> s2 = fmap (apply s1) s2 `union` s1

-- | Occurence check
(~>) :: Substable a => Var -> a -> Bool
var ~> a = var `S.member` ftv a


class Substable a where
    apply :: Subst -> a -> a
    -- | (f)ree (t)ype (v)ariables
    ftv :: a -> S.Set Var

instance Substable Type where
    apply s (Type nm types) =
        Type nm (apply s <$> types)
    apply s typ@(TypeVar nm) = findWithDefault typ nm s
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2
    apply s (TupleType types) = TupleType (apply s <$> types)
    apply s (ArrayType typ) = ArrayType (apply s typ)
    ftv (Type _ types) = fold (ftv <$> types)
    ftv (TypeVar nm) = S.singleton nm
    ftv (t1 :-> t2) = ftv t1 `S.union` ftv t2
    ftv (TupleType types) = foldl'
        (\tvs -> S.union tvs . ftv) S.empty types
    ftv (ArrayType typ) = ftv typ

instance Substable Scheme where
    apply s (Forall vars typ) =
        let s' = foldr delete s vars
        in Forall vars (apply s' typ)
    ftv (Forall vars typ) =
        ftv typ `S.difference` S.fromList vars

instance Substable a => Substable [a] where
    apply = fmap . apply
    ftv = foldr (S.union . ftv) S.empty

-- | For `TypeEnv` to avoid an orphan instance
instance Substable (VarMap Scheme) where
    apply = (<$>) . apply
    ftv = ftv . elems
