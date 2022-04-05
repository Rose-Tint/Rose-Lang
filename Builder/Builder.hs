{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Builder.Builder (
    BuilderT, Builder, BuilderIO, BuilderM,
    State(..),
    buildM, buildM_,
    liftBuild, (<#>),
    getCmdLine, getModule,
    getFilePath, setFilePath,
) where

import Control.Monad ((<$!>))
import Data.Functor.Identity (Identity)

import Builder.State
import CmdLine (CmdLine)
import Utils (pathToModule)


newtype BuilderT m a = Builder {
        unB :: forall b. State
            -> (a -> State -> m b)
            -> m b
    }

type Builder = BuilderT Identity
type BuilderIO = BuilderT IO
type BuilderM = BuilderT Maybe


instance Functor (BuilderT m) where
    fmap f b = Builder $ \s go ->
        unB b s (go . f)

instance Applicative (BuilderT m) where
    pure a = Builder $ \s go -> go a s
    fb <*> ab = do
        f <- fb
        a <- ab
        return $ f a

instance Monad (BuilderT m) where
    -- :: Builder m a -> (a -> Builder m b) -> Builder m b
    b >>= m = Builder $ \s go ->
        let go' !x s' = unB (m x) s' go
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
liftBuild m = Builder $ \s go -> m >>= (`go` s)

(<#>) :: Monad m => (a -> m b) -> a -> BuilderT m b
{-# INLINE (<#>) #-}
(<#>) = (liftBuild .)

getState :: BuilderT m State
{-# INLINE getState #-}
getState = Builder $ \s go -> go s s

modifyState :: (State -> State) -> BuilderT m ()
{-# INLINE modifyState #-}
modifyState f = Builder $ \s go -> go () (f s)

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