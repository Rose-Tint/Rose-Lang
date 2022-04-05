module Main (main) where

import Control.Monad (when)
import Data.Time (diffUTCTime, getCurrentTime)

import CmdLine (CmdLine(..), getCmdLine)
import Output (fatal, status)
import Build (build)
-- import Threading


default (Int, Double)



main :: IO ()
main = do
    cmdLine <- getCmdLine
    let verb = cmdVerb cmdLine
        errs = cmdErrors cmdLine
    when (not (null errs)) $
        fatal verb (concat errs) []

    timeStart <- getCurrentTime

    build cmdLine

    timeEnd <- getCurrentTime
    status verb "Finished in %s\n"
        [show (diffUTCTime timeEnd timeStart )]
