module Builder.Output (
    success, message, status, debug,
    trace,
    warn, fatal,
) where

import Control.Monad (when)
import System.Exit

import Builder.Internal
import Builder.CmdLine(
    CmdLine(..),
    isWEnabledFor, w_error
    )
import Pretty


default (Int, Double)


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
        (uncolor (detailed a))

myPutStr :: Int -> String -> BuilderIO ()
myPutStr thresh str = do
    verb <- cmdVerb . stCmdLine <$> getState
    when (verb >= thresh) $
        putStr <#> processString str
