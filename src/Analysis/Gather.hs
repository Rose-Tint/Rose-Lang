{- |
THIS MODULE IS NOT TO BE USED YET
-}
module Analysis.Gather () where

import Control.Monad.Trans.State

import AST
import Data.Table


data GState = GS {
    gsIndex :: !Int,
    gsTable :: !Table
    }

-- | Makes a pass to gather all identifiers
type Gather = StateT Table ()


class Gatherable a where
    gather :: a -> Gather ()

instance Gatherable Pattern where
    gather (CtorPtrn name args) = do
        types <- mapM (TypeVar . Var "" . getPos)
        let retT = TypeVar (Var "" (getPos name))
            typ = unfoldTypes types retT
        pushCtor name typ
    gather (TuplePtrn ptrns) = mapM_ gather ptrns
    gather _ = return ()

instance Gatherable Value where
    gather (VarVal name) =
        pushVar name (TypeVar (Var "" (getPos name)))
    gather (Application v1 v2) = gather v1 >> gather v2
