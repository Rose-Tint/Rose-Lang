module Builder.IO (
    bReadFile,
    createTraceDir,
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
        baseDir <- asks baseBuildDir
        modify $ \s -> s {
            filePath = path,
            moduleName = pathToModule path,
            currBuildDir = baseDir ++ pathToDir path,
            sourceCode = src
            }
        createTraceDir

createTraceDir :: Builder ()
createTraceDir = cmdTrace ??> do
    dir <- asks baseBuildDir
    io $ createDirectoryIfMissing True dir

message, status, debug
    :: Pretty a => a -> Builder ()
message = myPutStr 1 . terse
status = myPutStr 2 . pretty
debug = myPutStr 3 . detailed

warn :: String -> Builder ()
warn str = do
    wError ?!> fatal str
    myPutStr 1 str

fatal :: String -> Builder a
fatal str = do
    myPutStr 0 str
    io $ putChar '\n'
    io exitFailure

trace :: Pretty a => FilePath -> a -> Builder ()
trace path a = do
    dir <- asks baseBuildDir
    cmdTrace ??> io (writeFile
        (dir ++ path)
        (uncolor $! processString (detailed a)))

myPutStr :: Int -> String -> Builder ()
myPutStr thresh str = ((thresh <=).verbosity) ??> io
    (putStr (processString str))
