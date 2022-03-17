module Main (main) where

import Control.Monad (when)
import System.Exit (ExitCode(..))

import CmdLine (CmdLine(..), getCmdLine)
import Output (fatal)
import Build (build)
import Threading



main, main' :: IO ()
main = do
    mgr <- newManager
    thrID <- fork mgr main'
    wait mgr thrID
    return ()

main' = do
    cmdLine' <- getCmdLine
    let cmdLine = cmdLine' {
                cmdFiles = [
                        "Std/Data/Tree.th",
                        "Std/Memory.th",
                        "Std/Compare.th",
                        "Std/Maybe.th",
                        "Std/Bit.th",
                        "Std/Hash.th"
                    ],
                cmdVerb = 0,
                cmdTrace = False
            }
    let verb = cmdVerb cmdLine
        errs = cmdErrors cmdLine
    when (not (null errs)) $
        fatal verb (concat errs) []
    when (null (cmdFiles cmdLine)) $
        exitSelf ExitSuccess

    build cmdLine
