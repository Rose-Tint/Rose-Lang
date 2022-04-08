module Builder.Output (
    success, message, status, debug,
    trace,
    fatal,
) where

import Control.Monad (when, (<$!>))
import System.Exit

import Builder.Builder
import Builder.CmdLine
import CmdLine (CmdLine(cmdTrace))
import Color


default (Int, Double)


success, message, status, debug :: String -> BuilderIO ()
success = myPutStr 1
message = myPutStr 1
status = myPutStr 2
debug = myPutStr 3


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
