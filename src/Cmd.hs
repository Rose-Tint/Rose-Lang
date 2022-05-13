module Cmd (
    module Cmd.Flags,
    module Cmd.Warnings,
    CmdLine(..),
    readCmdLine,
) where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Cmd.Flags
import Cmd.Options
import Cmd.Warnings


default (Int, Double)


-- everything is strict for concurrency safety
data CmdLine = CmdLine {
        cmdFiles :: ![String],
        cmdVerb :: !Int,
        cmdBuildDir :: !FilePath,
        cmdTrace :: !Bool,
        cmdErrors :: ![String],
        cmdShadowing :: !Bool,
        cmdThreaded :: !Bool,
        cmdFlags :: !Flags,
        cmdWarns :: !Warning
    }


help :: IO Flag
help = do
    let header = "Usage: rose [FILES...] [OPTIONS...]"
    putStrLn $! usageInfo header optionOptions
    exitSuccess

setFlags :: [Flag] -> CmdLine -> CmdLine
{-# INLINABLE setFlags #-}
setFlags [] cmd = cmd
setFlags (flg:flgs) cmd = setFlags flgs $! case flg of
    Verbosity v -> cmd { cmdVerb = v }
    BuildDir dir -> cmd { cmdBuildDir = dir ++ "/" }
    Trace -> cmd { cmdTrace = True }
    Threaded -> cmd { cmdThreaded = True }
    Flag f -> cmd { cmdFlags = f (cmdFlags cmd) }
    Warn w -> cmd { cmdWarns = enableWarningFor w (cmdWarns cmd) }

mkCmdLine :: IO [Flag] -> [String] -> [String] -> IO CmdLine
mkCmdLine flgs fnames errs = do
    flgs' <- flgs
    let cmd = CmdLine {
            cmdFiles = fnames,
            cmdVerb = 1,
            cmdErrors = errs,
            cmdTrace = False,
            cmdBuildDir = "Rose-Build/",
            cmdShadowing = True,
            cmdThreaded = False,
            cmdFlags = f_default,
            cmdWarns = w_default
        }
    return $! setFlags flgs' cmd

options :: [OptDescr (IO Flag)]
options = [Option "h" ["help"] (NoArg help) "Displays help information"]
    ++ optionOptions
    ++ (fmap (pure . Warn) <$> warningOptions)
    ++ (fmap (pure . Flag) <$> flagOptions)

readCmdLine :: IO CmdLine
readCmdLine = do
    args <- reverse <$> getArgs
    let (opts, nons, errs) = getOpt RequireOrder options args
    mkCmdLine (reverse <$> sequence opts) nons errs
