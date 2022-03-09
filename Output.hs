{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Output (
    success,
    warn,
    message,
    status,
    info,
    debug,
    trace,
    fatal,
) where

import Control.Concurrent (forkIO)
import Data.List (foldl')
import System.Directory
import System.Exit (exitFailure)
import System.IO (Handle, stdout, stderr, hPutStr)

import CmdLine
import Color


success, warn, message, status, debug, info
    :: Int -> String -> [String] -> IO ()
success = myPutStr Green 1 stdout
warn = myPutStr Yellow 2 stderr
message = myPutStr Reset 1 stdout
status = myPutStr Reset 3 stdout
info = myPutStr Reset 4 stdout
debug = myPutStr Cyan 5 stderr


fatal :: Int -> String -> [String] -> IO a
fatal v str as = myPutStr Red 2 stderr v str as
    >> exitFailure


trace :: CmdLine -> FilePath -> String -> IO ()
trace cmd path str = forkIO (if cmdTrace cmd then do
        path' <- makeAbsolute path
        path'' <- makeRelativeToCurrentDirectory path'
        writeFile path'' str
    else
        return ()) >> return ()


myPutStr ::
    Color -> Int -> Handle -> Int -> String -> [String] -> IO ()
myPutStr clr thresh hdl vrb fStr args =
    if vrb >= thresh then
        -- i tried using stderr, but it printed weird
        -- characters and really slow
        hPutStr hdl $ foldl' printf fStr args `colored` clr
    else
        return ()
