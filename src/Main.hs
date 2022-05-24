module Main (main) where

import Control.Monad (unless, forM_)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Exit (exitFailure)

import Analysis
import Builder
import Cmd
import Common.Module
import Common.Var
import Parser
import Repl
import Text.Pretty
import Utils.FilePath (modToPath)


build :: Builder ()
build = do
    timeStart <- io getCurrentTime
    files <- asks cmdFiles
    mapM_ buildFile files
    timeEnd <- io getCurrentTime
    let diff = diffUTCTime timeEnd timeStart
    message ("Finished in "+|diff|+"\n")

buildFile :: FilePath -> Builder ()
buildFile [] = return ()
buildFile path = hasBeenVisited path >>= \skip ->
    if skip then return () else do
        bReadFile path
        name <- gets moduleName
        message ("Building Module ["+|name|+"]\n")
        Module imports tree <- parseFile
        _ <- runAnalysis tree
        forM_ imports $ \(Import (Var name' _)) ->
            buildFile (modToPath name')
        finalizeVisit

main :: IO ()
main = do
    (task, errs) <- readCmdLine
    unless (null errs) $ do
        mapM_ putStrLn errs
        exitFailure
    case task of
        Repl -> repl
        Build cmd -> runBuilder build cmd
