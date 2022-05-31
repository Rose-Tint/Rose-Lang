module Main (main) where

import Control.Monad (unless)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure)

import Cmd
import Repl
import Thorn
import Thorn.Project


runCompiler :: CmdLine -> IO ()
runCompiler cmd = runThorn cmd (mkProject (cmdFiles cmd))

main :: IO ()
main = do
    (cmd, task, errs) <- readCmdLine
    unless (null errs) $ do
        mapM_ putStrLn errs
        exitFailure
    time $ case task of
        Repl -> runRepl cmd
        Build -> buildWithThorn cmd
        NoTask -> runCompiler cmd

time :: IO a -> IO a
time io = do
    start <- getCurrentTime
    x <- io
    end <- getCurrentTime
    let diff = diffUTCTime end start
    putStrLn ("Finished in " ++ show diff)
    return x
