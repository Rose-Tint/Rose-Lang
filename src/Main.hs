module Main (main) where

import Control.Monad (unless)
import Data.Time (diffUTCTime, getCurrentTime)

import Analysis
import Builder
import Parser
import Pretty
import Utils.FilePath (modToPath)


default (Int, Double)


main :: IO ()
main = do
    cmd <- readCmdLine
    let errs = cmdErrors cmd
    unless (null errs) $
        putStrLn (concat errs)
    timeStart <- getCurrentTime
    buildM build cmd
    timeEnd <- getCurrentTime
    unless (cmdVerb cmd <= 0) $
        "Finished in "+|diffUTCTime timeEnd timeStart+|"\n"
