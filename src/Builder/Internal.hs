{-# LANGUAGE Rank2Types #-}

module Builder.Internal (
    gets,
    modify,
    asks,

    Builder,
    Stream,
    runBuilder,
    (??>),
    (?!>),
    io,
    hasBeenVisited,
    finalizeVisit,
    filePath,
    moduleName,
    currBuildDir,
    visitedFiles,
    sourceCode,
) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (gets, modify)
import qualified Control.Monad.Trans.State as S (
    gets,
    modify,
    )
import Data.Set (Set, empty, insert, member)

import Cmd


type Stream = String

data BldState = BldState {
        filePath :: FilePath,
        moduleName :: String,
        -- current build directory, as opposed to
        -- cmdBuildDir, which is the base
        currBuildDir :: FilePath,
        -- list of visited files
        visitedFiles :: Set FilePath,
        sourceCode :: Stream
    }

type Builder a = ReaderT
    CmdLine
    (StateT BldState IO)
    a


newState :: BldState
newState = BldState [] [] [] empty ""

runBuilder :: Builder a -> CmdLine -> IO a
runBuilder bld cmd = evalStateT (runReaderT bld cmd) newState

infixr 9 ??>
(??>) :: (CmdLine -> Bool) -> Builder a -> Builder ()
f ??> bld = do
    flg <- asks f
    when flg (bld >> return ())

infix 9 ?!>
(?!>) :: Warning -> Builder a -> Builder ()
(?!>) wrn = ((isWEnabledFor wrn . cmdWarns) ??>)

io :: IO a -> Builder a
io = lift . lift

gets :: (BldState -> a) -> Builder a
gets = lift . S.gets

modify :: (BldState -> BldState) -> Builder ()
modify = lift . S.modify

hasBeenVisited :: FilePath -> Builder Bool
hasBeenVisited path = do
    visited <- gets visitedFiles
    return (path `member` visited)

finalizeVisit :: Builder ()
finalizeVisit = do
    baseDir <- asks cmdBuildDir
    modify $ \s -> s {
        visitedFiles = insert (filePath s) (visitedFiles s),
        sourceCode = "",
        currBuildDir = baseDir
        }
    return ()
