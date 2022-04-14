module Builder.Output (
    success, message, status, debug,
    trace,
    warn, fatal,
) where

import Control.Monad (when, (<$!>))
import System.Exit

import Builder.Builder
import Builder.CmdLine
import CmdLine (
    CmdLine(cmdTrace, cmdWarns),
    isWEnabledFor, w_error
    )
import Color


default (Int, Double)


success, message, status, debug :: String -> BuilderIO ()
success = myPutStr 1
message = myPutStr 1
status = myPutStr 2
debug = myPutStr 3

warn :: String -> BuilderIO ()
warn str = do
    ws <- cmdWarns <$!> getCmdLine
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

trace :: FilePath -> String -> BuilderIO ()
trace path str = do
    doTrace <- cmdTrace <$!> getCmdLine
    dir <- getBuildDir
    when doTrace <#>
        writeFile (dir ++ path) (uncolor str)

myPutStr :: Int -> String -> BuilderIO ()
myPutStr thresh str = do
    verb <- getVerbosity
    -- `when` doesnt work?
    if (verb >= thresh) then
        putStr <#> color str
    else
        return ()
