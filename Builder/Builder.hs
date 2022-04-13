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
    addUTDModule, isUpToDate,
) where

import Control.Monad ((<$!>))
import Data.Functor.Identity (Identity)
import Data.Set (member, insert)

import Builder.State
import CmdLine (CmdLine(cmdBuildDir))
import Utils (pathToModule, pathToDir)


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
    {-# INLINE fmap #-}
    fmap f b = Builder $ \ !s go ->
        unB b s (go . f)

instance Applicative (BuilderT m) where
    {-# INLINE pure #-}
    pure a = Builder $ \ !s !go -> go a s
    {-# INLINE (<*>) #-}
    fb <*> ab = fb >>= (<$!> ab)

instance Monad (BuilderT m) where
    -- :: Builder m a -> (a -> Builder m b) -> Builder m b
    {-# INLINE (>>=) #-}
    b >>= m = Builder $ \ !s !go ->
        let go' x s' = let !x' = m x in unB x' s' go
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
    base <- cmdBuildDir <$> getCmdLine
    modifyState (\s -> s {
            stBuildDir = base ++ pathToDir path
        })

getBuildDir :: BuilderT m FilePath
{-# INLINE getBuildDir #-}
getBuildDir = stBuildDir <$!> getState

setSource :: Stream -> BuilderT m ()
{-# INLINE setSource #-}
setSource t = modifyState (\s -> s { stSource = t })

getSource :: BuilderT m Stream
{-# INLINE getSource #-}
getSource = stSource <$!> getState

addUTDModule :: String -> BuilderT m ()
{-# INLINE addUTDModule #-}
addUTDModule name = modifyState $ \s ->
    s { stUTDModules = insert name (stUTDModules s) }

-- filterUTDs :: [String] -> BuilderT m [String]
-- {-# INLINABLE filterUTDs #-}
-- filterUTDs names = do
--     utds <- stUTDModules <$!> getState
--     return $! go names utds
--     where
--         go [] _ = []
--         go (m:ms) utds = if member m utds then
--                 go ms utds
--             else
--                 (m:go ms utds)

isUpToDate :: String -> BuilderT m Bool
{-# INLINE isUpToDate #-}
isUpToDate name = member name . stUTDModules <$> getState
