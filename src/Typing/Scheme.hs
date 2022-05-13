{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Typing.Scheme (
    Scheme(..),
) where

import Common.Var
import Typing.Type


-- http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- haskell example: `forall a b. a -> b`
-- using set-symbols: ∀ab. a -> b
data Scheme = Forall [Var] Type
