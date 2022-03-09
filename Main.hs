module Main (main) where

-- import Control.Concurrent (forkIO)
import Control.Monad (foldM_, when)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Exit (exitSuccess)

import CmdLine (CmdLine(..), getCmdLine)
import Output (fatal, status)
import Build (buildFile)



main :: IO ()
main = do
    cmdLine <- getCmdLine
    let verb = cmdVerb cmdLine
        errs = cmdErrors cmdLine
    when (not (null errs))
        (fatal verb (concat errs) [])
    when (null (cmdFiles cmdLine))
        exitSuccess

    timeStart <- getCurrentTime

    -- foldM_ (\_ a -> forkIO (buildFile cmdLine a)
    --             >> return ())
    --     () (cmdFiles cmdLine)
    foldM_ (\_ -> buildFile cmdLine)
        () (cmdFiles cmdLine)

    timeEnd <- getCurrentTime
    status verb "Finished in %s\n"
        [show (diffUTCTime timeEnd timeStart )]
    exitSuccess
