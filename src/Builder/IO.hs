module Builder.IO (
    mReadFile,
    createTraceDir,
) where

import System.Directory (createDirectoryIfMissing)

import Builder.CmdLine
import Builder.Internal
import Utils.Paths


mReadFile :: FilePath -> BuilderIO (Maybe String)
mReadFile path = do
    skip <- hasBeenVisited path
    if skip then do
        -- module has already been parsed
        updateState $ \s -> s { stSource = "" }
        return Nothing
    else do
        src <- readFile <#> path
        baseDir <- cmdBuildDir . stCmdLine <$> getState
        updateState $ \s -> s {
            stFile = path,
            stModule = pathToModule path,
            stBuildDir = baseDir ++ pathToDir path,
            stSource = src
            }
        createTraceDir
        return (Just src)

createTraceDir :: BuilderIO (Maybe FilePath)
createTraceDir = do
    state <- getState
    let dir = stBuildDir state
    if cmdTrace (stCmdLine state) then do
        createDirectoryIfMissing True <#> dir
        return (Just dir)
    else
        return Nothing
