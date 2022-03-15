module Main (main) where

import Control.Monad (when)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Exit (exitSuccess)

import CmdLine (CmdLine(..), getCmdLine)
import Output (fatal, status)
import Build (build)
import Threading



main, main' :: IO ()
main = do
    mgr <- newManager
    thrID <- fork mgr main'
    wait mgr thrID
    return ()
main' = do
    cmdLine <- getCmdLine
    let verb = cmdVerb cmdLine
        errs = cmdErrors cmdLine
    when (not (null errs))
        (fatal verb (concat errs) [])
    when (null (cmdFiles cmdLine))
        exitSuccess

    timeStart <- getCurrentTime

    build cmdLine

    timeEnd <- getCurrentTime
    status verb "Finished in %s\n"
        [show (diffUTCTime timeEnd timeStart )]
