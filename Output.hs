{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Output (
    success,
    warn,
    message,
    status,
    debug,
    trace,
    fatal,
) where

import Control.Concurrent (forkIO)
import Data.List (foldl')
import System.Directory
import System.Exit (exitFailure)

import CmdLine
import Color


success, warn, message, status, debug
    :: Int -> String -> [String] -> IO ()
success = myPutStr Green 2
warn = myPutStr Yellow 2
message = myPutStr Reset 2
status = myPutStr Reset 3
debug = myPutStr Cyan 4

fatal :: Int -> String -> [String] -> IO a
fatal v str as = myPutStr Red 2 v str as
    >> exitFailure


trace :: CmdLine -> FilePath -> String -> IO ()
trace cmd path str = forkIO (if cmdTrace cmd then do
        path' <- makeAbsolute path
        path'' <- makeRelativeToCurrentDirectory path'
        writeFile path'' str
    else
        return ()) >> return ()


myPutStr ::
    Color -> Int -> Int -> String -> [String] -> IO ()
myPutStr clr thresh vrb fStr args =
    if vrb >= thresh then
        -- i tried using stderr, but it printed weird
        -- characters and really slow
        putStr $ foldl' printf fStr args `colored` clr
    else
        return ()
