{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Output where

{-
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
import System.IO (Handle, stdout, stderr, hPutStr)

import CmdLine
import Color
import Threading


default (Int, Double)


success, warn, message, status, debug, info
    :: CmdLine -> String -> [String] -> IO ()
success = myPutStr Green 1 stdout
warn = myPutStr Yellow 2 stdout -- stderr
message = myPutStr Reset 1 stdout
status = myPutStr Reset 3 stdout
info = myPutStr Reset 4 stdout
debug = myPutStr Cyan 5 stdout -- stderr


fatal :: Int -> String -> [String] -> IO a
fatal v str as = myPutStr Red 2 stderr v str as
    >> exitSelf (ExitFailure 1)


trace :: CmdLine -> FilePath -> String -> IO ()
trace cmd path str = forkIO (if cmdTrace cmd then do
        path' <- makeAbsolute path
        path'' <- makeRelativeToCurrentDirectory path'
        writeFile path'' str
    else
        return ()) >> return ()


myPutStr :: Color -> Int -> Handle -> CmdLine -> String -> [String] -> IO ()
myPutStr clr thresh hdl cmd fStr args =
    if cmdVerb cmd >= thresh then
        -- i tried using stderr, but it was slow and raw
        hPutStr hdl $ foldl' printf fStr args `colored` clr
    else
        return ()
-}