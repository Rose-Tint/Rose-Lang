{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Builder.Builder (
    BuilderT, Builder, BuilderIO, BuilderM,
    State(..),
    buildM, buildM_,
    liftBuild, (<#>),
    getCmdLine, getModule,
    getFilePath, setFilePath,
    setSource, getSource,
    setBuildDir, getBuildDir,
) where

import Control.Monad ((<$!>))
import Data.Functor.Identity (Identity)

import Builder.State
import CmdLine (CmdLine(cmdBuildDir))
import Utils (pathToModule)


-- TODO: Decide if this should also be in the
-- IO Monad (m (IO b) for the addition of a
-- thread manager
newtype BuilderT m a = Builder {
        unB :: forall b. State
            -> (a -> State -> m b)
            -> m b
    }

type Builder = BuilderT Identity
type BuilderIO = BuilderT IO
type BuilderM = BuilderT Maybe


instance Functor (BuilderT m) where
    {-# INLINE fmap #-}
    fmap f b = Builder $ \ !s go ->
        unB b s (go . f)

instance Applicative (BuilderT m) where
    {-# INLINE pure #-}
    pure a = Builder $ \ !s go -> go a s
    {-# INLINE (<*>) #-}
    fb <*> ab = fb >>= (<$!> ab)

instance Monad (BuilderT m) where
    -- :: Builder m a -> (a -> Builder m b) -> Builder m b
    {-# INLINE (>>=) #-}
    b >>= m = Builder $ \ !s go ->
        let go' !x s' = let !x' = m x in unB x' s' go
        in unB b s go'


buildM :: Monad m => BuilderT m a -> CmdLine -> m a
{-# INLINE buildM #-}
buildM (Builder b) !cmd = b (mkState cmd) go
    where
        go x _ = return x

buildM_ :: Monad m => BuilderT m a -> CmdLine -> m ()
{-# INLINE buildM_ #-}
buildM_ (Builder b) !cmd = b (mkState cmd) go
    where
        go _ _ = return ()

liftBuild :: Monad m => m a -> BuilderT m a
{-# INLINE liftBuild #-}
liftBuild m = Builder $ \ !s go -> m >>= (`go` s)

(<#>) :: Monad m => (a -> m b) -> a -> BuilderT m b
{-# INLINE (<#>) #-}
(<#>) = (liftBuild .)

getState :: BuilderT m State
{-# INLINE getState #-}
getState = Builder $ \ !s go -> go s s

modifyState :: (State -> State) -> BuilderT m ()
{-# INLINE modifyState #-}
modifyState f = Builder $ \ !s go -> go () (f s)

getCmdLine :: BuilderT m CmdLine
{-# INLINE getCmdLine #-}
getCmdLine = stCmdLine <$!> getState

getFilePath :: BuilderT m FilePath
{-# INLINE getFilePath #-}
getFilePath = stFile <$!> getState

getModule :: BuilderT m String
{-# INLINE getModule #-}
getModule = stModule <$!> getState

setFilePath :: FilePath -> BuilderT m ()
{-# INLINE setFilePath #-}
setFilePath p = modifyState (\s -> s {
        stFile = p,
        stModule = pathToModule p
    })

setBuildDir :: FilePath -> BuilderT m ()
{-# INLINE setBuildDir #-}
setBuildDir path = do
    cmd <- getCmdLine
    let base = cmdBuildDir cmd
    modifyState (\s -> s { stBuildDir = base ++ path })

getBuildDir :: BuilderT m FilePath
{-# INLINE getBuildDir #-}
getBuildDir = stBuildDir <$!> getState

setSource :: Stream -> BuilderT m ()
{-# INLINE setSource #-}
setSource t = modifyState (\s -> s { stSource = t })

getSource :: BuilderT m Stream
{-# INLINE getSource #-}
getSource = stSource <$!> getState
