module Main (main) where

import Control.Monad (unless)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Exit (exitFailure)

import Analysis
import AST (ParseTree(..))
import Builder
import Cmd
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
        ParseTree imports tree <- parseFile
        _ <- runAnalysis tree
        mapM_ (buildFile.modToPath.varName) imports
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
