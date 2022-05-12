module Builder.IO (
    mReadFile,
    createTraceDir,
) where

import System.Directory (createDirectoryIfMissing)

import Builder.Internal
import Cmd
import Utils.Paths
import Pretty


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

success, message, status, debug
    :: Pretty a => a -> BuilderIO ()
success = myPutStr 1 . terse
message = myPutStr 1 . terse
status = myPutStr 2 . pretty
debug = myPutStr 3 . detailed

warn :: String -> BuilderIO ()
warn str = do
    ws <- cmdWarns . stCmdLine <$> getState
    -- -Werror sets negative
    if w_error `isWEnabledFor` ws then
        fatal str
    else
        myPutStr 1 str

fatal :: String -> BuilderIO a
fatal str = do
    myPutStr 0 str
    putChar <#> '\n'
    liftBuild exitFailure

trace :: Pretty a => FilePath -> a -> BuilderIO ()
trace path a = do
    doTrace <- cmdTrace . stCmdLine <$> getState
    dir <- stBuildDir <$> getState
    when doTrace <#> writeFile
        (dir ++ path)
        (uncolor (processString (detailed a)))

myPutStr :: Int -> String -> BuilderIO ()
myPutStr thresh str = do
    verb <- cmdVerb . stCmdLine <$> getState
    when (verb >= thresh) $
        putStr <#> processString str
