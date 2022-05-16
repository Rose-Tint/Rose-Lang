module Control.Monad.Trans.StateReader (

) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


type StateReaderT r s m a = ReaderT r (StateT s m) a
