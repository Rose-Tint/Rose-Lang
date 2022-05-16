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


build :: Builder ()
build = asks cmdFiles >>= mapM_ buildFile

buildFile :: FilePath -> Builder ()
buildFile [] = return ()
buildFile path = hasBeenVisited path >>= \skip ->
    if skip then return () else do
        bReadFile path
        name <- gets moduleName
        message ("Building Module ["+|name|+"]\n")
        Module imports tree <- parseFile
        forM_ imports $ \(Import (Var modName _) _) ->
            buildFile (modToPath modName)
        (_, _) <- runAnalysis tree
        finalizeVisit

main :: IO ()
main = do
    cmd <- readCmdLine
    let errs = cmdErrors cmd
    unless (null errs) $
        putStrLn (concat errs)
    timeStart <- getCurrentTime
    runBuilder build cmd
    timeEnd <- getCurrentTime
    unless (verbosity cmd <= 0) $ putStrLn $
        "Finished in "+|diffUTCTime timeEnd timeStart|+"\n"
