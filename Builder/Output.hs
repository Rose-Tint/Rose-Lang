module Builder.Output (
    success, message, status, debug,
    trace,
    fatal,
) where

import Control.Monad (when, (<$!>))
import Data.List (foldl')
import System.Exit

import Builder.Builder
import Builder.CmdLine
import CmdLine (CmdLine(cmdTrace))
import Color


default (Int, Double)


success, message, status, debug
    :: String -> [String] -> BuilderIO ()
success = myPutStr 1
message = myPutStr 1
status = myPutStr 2
debug = myPutStr 3


fatal :: String -> [String] -> BuilderIO a
fatal str as = do
    myPutStr 0 str as
    liftBuild exitFailure


trace :: FilePath -> String -> BuilderIO ()
trace path str = do
    doTrace <- cmdTrace <$!> getCmdLine
    dir <- getBuildDir
    when doTrace <#>
        writeFile (dir ++ path) (uncolor str)


myPutStr :: Int -> String -> [String] -> BuilderIO ()
myPutStr thresh str args = do
    verb <- getVerbosity
    -- `when` doesnt work?
    if (verb >= thresh) then
    -- doesn't fold properly?
        putStr <#> foldl printf str args
    else
        return ()


printfHelper :: String -> [String] -> String
printfHelper [] _ = []
printfHelper 
