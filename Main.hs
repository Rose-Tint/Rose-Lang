module Main (main) where

import Control.Monad (when)
import Data.Time (diffUTCTime, getCurrentTime)

import CmdLine (CmdLine(..), getCmdLine)
import Output (fatal, status)
import Build (build)
import Threading


default (Int, Double)



main :: IO ()
main = do
    cmdLine <- getCmdLine
    let verb = cmdVerb cmdLine
        errs = cmdErrors cmdLine
    when (not (null errs)) $
        fatal verb (concat errs) []
    -- when (null (cmdFiles cmdLine)) $! do
    --     exitSelf ExitSuccess

    timeStart <- getCurrentTime

    -- main thread is slow in concurrency because
    -- it is an OS thread, not a haskell-runtime
    -- thread
    _ <- if cmdThreaded cmdLine then do
        mgr <- newManager
        tid <- fork mgr $ build cmdLine
        wait mgr tid
        return ()
    else do
        build cmdLine
        return ()

    timeEnd <- getCurrentTime
    status verb "Finished in %s\n"
        [show (diffUTCTime timeEnd timeStart )]
