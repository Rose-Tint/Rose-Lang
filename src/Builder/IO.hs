module Builder.IO (
    bReadFile,
    createTraceDir,
    success,
    message,
    status,
    debug,
    warn,
    fatal,
    trace,
) where

import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing)

import Builder.Internal
import Cmd
import Utils.FilePath
import Text.Pretty


bReadFile :: FilePath -> Builder ()
bReadFile path = do
    skip <- hasBeenVisited path
    if skip then modify $ \s -> s { sourceCode = "" }
    else do
        src <- io $ readFile path
        baseDir <- asks cmdBuildDir
        modify $ \s -> s {
            filePath = path,
            moduleName = pathToModule path,
            currBuildDir = baseDir ++ pathToDir path,
            sourceCode = src
            }
        createTraceDir

createTraceDir :: Builder ()
createTraceDir = cmdTrace ??> do
    dir <- gets currBuildDir
    io $ createDirectoryIfMissing True dir

success, message, status, debug
    :: Pretty a => a -> Builder ()
success = myPutStr 1 . terse
message = myPutStr 1 . terse
status = myPutStr 2 . pretty
debug = myPutStr 3 . detailed

warn :: String -> Builder ()
warn str = do
    w_error ?!> fatal str
    myPutStr 1 str

fatal :: String -> Builder a
fatal str = do
    myPutStr 0 str
    io $ putChar '\n'
    io exitFailure

trace :: Pretty a => FilePath -> a -> Builder ()
trace path a = do
    dir <- gets currBuildDir
    cmdTrace ??> io (writeFile
        (dir ++ path)
        (uncolor $! processString (detailed a)))

myPutStr :: Int -> String -> Builder ()
myPutStr thresh str = ((thresh <=).cmdVerb) ??> io
    (putStr (processString str))
