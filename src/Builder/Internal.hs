{-# LANGUAGE Rank2Types #-}

module Builder.Internal (
    gets,
    modify,
    asks,
    ask,

    Builder,
    Stream,
    runBuilder,

    (??>),
    (?!>),
    io,

    loadObjectFile,
    objFileExists,
    modToObjPath,

    getDir,
    getCurrTraceDir,
    getBinDir,

    moduleName,
    sourceCode,
    buildDirectory,
    sourceDirectory,
) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (gets, modify)
import qualified Control.Monad.Trans.State as S (
    gets,
    modify,
    )
import Data.Binary (decodeFile)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    )

import Cmd
import Common.Module
import Data.Table


type Stream = String

data BldState = BldState {
        moduleName :: ModName,
        sourceCode :: Stream,
        buildDirectory :: FilePath,
        sourceDirectory :: FilePath
    }

type Builder a = ReaderT
    CmdLine
    (StateT BldState IO)
    a


newState :: BldState
newState = BldState (End "") "" "" ""

runBuilder :: Builder a -> CmdLine -> IO a
runBuilder bld cmd = evalStateT (runReaderT bld cmd) newState

infixr 9 ??>
(??>) :: (CmdLine -> Bool) -> Builder a -> Builder ()
f ??> bld = do
    flg <- asks f
    when flg (bld >> return ())

infix 9 ?!>
(?!>) :: (Warnings -> Bool) -> Builder a -> Builder ()
(?!>) wrn = ((wrn . warnings) ??>)

io :: IO a -> Builder a
io = lift . lift

gets :: (BldState -> a) -> Builder a
gets = lift . S.gets

modify :: (BldState -> BldState) -> Builder ()
modify = lift . S.modify

loadObjectFile :: ModName -> Builder (Maybe Table)
loadObjectFile name = do
    exists <- objFileExists name
    if exists then do
        path <- modToObjPath name
        tbl <- io (decodeFile path)
        return (Just tbl)
    else
        return Nothing

objFileExists :: ModName -> Builder Bool
objFileExists name = do
    path <- modToObjPath name
    exists <- io (doesFileExist path)
    return exists

modToObjPath :: ModName -> Builder FilePath
modToObjPath name = do
    bin <- getBinDir
    let path = bin ++ modToContFile name ".o"
    return $! path

getDir :: String -> Builder FilePath
getDir str = do
    base <- asks baseBuildDir
    let !dir = base ++ "/" ++ str ++ "/"
    io (createDirectoryIfMissing True dir)
    return dir

getCurrTraceDir :: Builder FilePath
getCurrTraceDir = do
    curr <- gets (modToDir . moduleName)
    getDir ("trace/" ++ curr)

getBinDir :: Builder FilePath
getBinDir = getDir "bin"
