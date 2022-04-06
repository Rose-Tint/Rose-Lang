module Builder.Output (
    success,
    warn,
    message,
    status,
    info,
    debug,
    trace,
    fatal,
) where

import Control.Monad (when)
import Data.List (foldl')
import System.Exit

import Builder.Builder
import Builder.CmdLine
import Color


default (Int, Double)


success, warn, message, status, debug, info
    :: String -> [String] -> BuilderIO ()
success = myPutStr 1
warn = myPutStr 2
message = myPutStr 1
status = myPutStr 3
debug = myPutStr 4
info = myPutStr 4


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
    when (verb >= thresh)
        putStr <#> foldl' printf str args

