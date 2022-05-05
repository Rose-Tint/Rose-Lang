module Main (main) where

import Control.Monad (unless)
import Data.Time (diffUTCTime, getCurrentTime)

import Builder
import Builder.CmdLine
import Pretty (printf)


default (Int, Double)


main :: IO ()
main = do
    cmd <- readCmdLine
    let errs = cmdErrors cmd
    unless (null errs) $
        putStrLn (concat errs)
    timeStart <- getCurrentTime
    buildM_ build cmd
    timeEnd <- getCurrentTime
    unless (cmdVerb cmd <= 0) $ printf
        "Finished in %s\n"
        (show (diffUTCTime timeEnd timeStart))
