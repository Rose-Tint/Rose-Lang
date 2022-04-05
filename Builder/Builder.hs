{-# LANGUAGE Rank2Types #-}

module Builder.Builder where

import Control.Monad ((<$!>))
import Data.Functor.Identity (Identity)

import CmdLine (CmdLine)
import Threading


newtype BuilderT m a = Builder {
        unB :: forall b. CmdLine -> ThreadMgr
                (a -> CmdLine -> m b)
            -> m b
    }

type Builder = BuilderT Identity

type BuilderIO = BuilderT IO


instance Functor (BuilderT m) where
    fmap f b = Builder $ \cmd go ->
        unB b cmd (go . f)

instance Applicative (BuilderT m) where
    pure a = Builder $ \cmd go -> go a cmd
    fb <*> ab = do
        f <- fb
        a <- ab
        return $ f a

instance Monad (BuilderT m) where
    return = pure
    -- :: Builder m a -> (a -> Builder m b) -> Builder m b
    b >>= m = Builder $ \cmd go ->
        let go' !x cmd' = unB (m x) cmd' go
        in unB b cmd go'


buildM :: Monad m => BuilderT m a -> CmdLine -> m a
{-# INLINE buildM #-}
buildM b !cmd = unB b cmd (\x _ -> return x)


buildM_ :: Monad m => BuilderT m a -> CmdLine -> m ()
{-# INLINE buildM_ #-}
buildM_ b !cmd = unB b cmd (\_ _ -> return ())


getCmdLine :: BuilderT m CmdLine
{-# INLINE getCmdLine #-}
getCmdLine = Builder $ \cmd go -> go cmd cmd


fromCmd :: (CmdLine -> a) -> BuilderT m a
{-# INLINE fromCmd #-}
fromCmd f = f <$!> getCmdLine
