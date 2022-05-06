{-# LANGUAGE Rank2Types #-}

module Builder.Internal (
    BuilderT, Builder, BuilderIO, BuilderM,
    State(..),
    buildM,
    liftBuild, (<#>),
    getState, updateState,
    getModule, getSource,
    hasBeenVisited,
    finalizeVisit,
) where

import Control.Monad ((<$!>))
import Data.Functor.Identity (Identity)
import Data.Set (insert, member)

import Builder.CmdLine.Internal
import Builder.State


-- TODO: Decide if this should also be in the
-- IO Monad (m (IO b) for the addition of a
-- thread manager
-- UPDATE: i tried it. i had file writing and
-- console output launching in new threads that
-- are waited for after the Builder runs (i
-- _did_ forget to try to launch each building
-- file in a new thread, but that can be easily
-- done without any modifications to BuilderT).
-- it was slower (if only a little bit, yet
-- observably and consistently).
newtype BuilderT m a = Builder {
        unB :: forall b. State
            -> (a -> State -> m b)
            -> m b
    }

type Builder = BuilderT Identity
type BuilderIO = BuilderT IO
type BuilderM = BuilderT Maybe


instance Functor (BuilderT m) where
    fmap f b = Builder $ \ !s go ->
        unB b s (go . f)

instance Applicative (BuilderT m) where
    pure a = Builder $ \ !s !go -> go a s
    fb <*> ab = fb >>= (<$!> ab)

instance Monad (BuilderT m) where
    -- :: Builder m a -> (a -> Builder m b) -> Builder m b
    b >>= m = Builder $ \ !s !go ->
        let go' x s' = let !x' = m x in unB x' s' go
        in unB b s go'


buildM :: Monad m => BuilderT m a -> CmdLine -> m a
buildM (Builder b) !cmd = b (mkState cmd) go
    where
        go x _ = return x

liftBuild :: Monad m => m a -> BuilderT m a
liftBuild m = Builder $ \ !s go -> m >>= (`go` s)

(<#>) :: Monad m => (a -> m b) -> a -> BuilderT m b
(<#>) = (liftBuild .)

getState :: BuilderT m State
getState = Builder $ \ !s go -> go s s

updateState :: (State -> State) -> BuilderT m ()
updateState f = Builder $ \ !s go -> go () (f s)

getModule :: BuilderT m String
getModule = stModule <$!> getState

getSource :: BuilderT m Stream
getSource = stSource <$!> getState

hasBeenVisited :: FilePath -> BuilderT m Bool
hasBeenVisited path = do
    visited <- stVisited <$> getState
    return (path `member` visited)

finalizeVisit :: BuilderT m ()
finalizeVisit = do
    path <- stFile <$> getState
    updateState $ \s ->
        s { stVisited = insert path (stVisited s) }
