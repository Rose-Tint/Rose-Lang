{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Typing.Scheme (
    Scheme(..),
) where

import Common.Var
import Text.Pretty
import Typing.Type


-- http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- haskell example: `forall a b. a -> b`
data Scheme = Forall [Var] Type


instance Pretty Scheme where
    terse (Forall _ typ) = terse typ
    pretty (Forall [] typ) = pretty typ
    pretty (Forall vs typ) = 
        "forall "+|" "`seps`vs|+" . "+|typ
