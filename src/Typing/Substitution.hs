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

import Data.Foldable (fold)
import qualified Data.Set as S

import Data.VarMap
import Common.Var
import Data.Table
import Typing.Scheme
import Typing.Type


type Subst = VarMap Type


nullSubst :: Subst
nullSubst = empty

compose :: Foldable t => t Subst -> Subst
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
    apply s (TypeCon nm types) = TypeCon nm (apply s <$> types)
    apply s typ@(TypeVar nm) = findWithDefault typ nm s
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2
    apply s (TupleType types) = TupleType (apply s <$> types)
    apply s (ArrayType typ) = ArrayType (apply s typ)
    ftv (TypeCon _ types) = foldMap ftv types
    ftv (TypeVar nm) = S.singleton nm
    ftv (t1 :-> t2) = ftv t1 `S.union` ftv t2
    ftv (TupleType types) = foldMap ftv types
    ftv (ArrayType typ) = ftv typ

instance Substable Scheme where
    apply s (Forall vars typ) =
        let s' = foldr delete s vars
        in Forall vars (apply s' typ)
    ftv (Forall vars typ) =
        ftv typ `S.difference` S.fromList vars

instance Substable a => Substable [a] where
    apply = fmap . apply
    ftv = foldMap ftv

instance (Substable a, Substable b) => Substable (a, b) where
    apply s (a, b) = (apply s a, apply s b)
    ftv (a, b) = ftv a <> ftv b

instance Substable a => Substable (VarMap a) where
    apply = fmap . apply
    ftv = foldMap ftv

instance Substable Func where
    apply sub func = func { funcType = apply sub (funcType func) }
    ftv = ftv . funcType

instance Substable Table where
    apply sub (Table types trts glbs scps) =
        Table types trts
        (apply sub <$> glbs)
        (fmap (apply sub) <$> scps)
    ftv (Table _ _ glbs scps) =
        foldMap ftv (glbs:scps)
