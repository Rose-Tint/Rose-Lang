module Main (main) where

import Control.Monad (unless, forM_)
import Data.Time (diffUTCTime, getCurrentTime)

import Analysis
import Builder
import Cmd
import Common.Module
import Common.Var
import Parser
import Text.Pretty
import Utils.FilePath (modToPath)


build :: BuilderIO ()
build = do
    cmd <- stCmdLine <$> getState
    mapM_ buildFile (cmdFiles cmd)

buildFile :: FilePath -> BuilderIO ()
buildFile [] = return ()
buildFile path = hasBeenVisited path >>= \skip ->
    if skip then return () else do
        mReadFile path
        name <- stModule <$> getState
        message ("Building Module ["+|name|+"]\n")
        Module imports tree <- parseFile
        forM_ imports $ \(Import (Var modName _) _) ->
            buildFile (modToPath modName)
        _ <- runAnalysis tree
        finalizeVisit

main :: IO ()
main = do
    cmd <- readCmdLine
    let errs = cmdErrors cmd
    unless (null errs) $
        putStrLn (concat errs)
    timeStart <- getCurrentTime
    buildM build cmd
    timeEnd <- getCurrentTime
    unless (cmdVerb cmd <= 0) $ putStrLn $
        "Finished in "+|diffUTCTime timeEnd timeStart|+"\n"
