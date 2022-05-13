{-# LANGUAGE Rank2Types #-}

module Builder.Internal (
    BuilderT,
    Builder,
    BuilderIO,
    Stream,
    State(..),
    mkState,
    buildM,
    liftBuild, (<#>),
    getState, updateState,
    getModule, getSource,
    hasBeenVisited,
    finalizeVisit,
) where

import Control.Monad ((<$!>))
import Data.Functor.Identity (Identity)
import Data.Set (Set, empty, insert, member)

import Cmd


type Stream = String

data State = State {
        stCmdLine :: CmdLine,
        stFile :: FilePath,
        stModule :: String,
        -- current build directory, as opposed to
        -- cmdBuildDir, which is the base
        stBuildDir :: FilePath,
        -- list of visited files
        stVisited :: Set FilePath,
        stSource :: String
    }

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


mkState :: CmdLine -> State
{-# INLINE mkState #-}
mkState cmd = State cmd [] [] [] empty ""

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
    baseDir <- cmdBuildDir . stCmdLine <$> getState
    updateState $ \s -> s {
        stVisited = insert (stFile s) (stVisited s),
        stSource = "",
        stBuildDir = baseDir
        }
