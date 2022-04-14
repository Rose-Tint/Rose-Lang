module CmdLine (
    module CmdLine.Flags,
    module CmdLine.Warnings,
    CmdLine(..),
    getCmdLine
) where

import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import CmdLine.Flags
import CmdLine.Warnings

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

data Flag
    = Verbosity Int
    | BuildDir FilePath
    | Trace
    | Threaded
    | Flag (Flags -> Flags)
    | Warn Warning


verbosity :: Maybe String -> IO Flag
verbosity Nothing = return (Verbosity 3)
verbosity (Just str) = return (Verbosity (read str))

silent :: IO Flag
silent = return (Verbosity 0)

debug_info :: IO Flag
debug_info = return (Verbosity 5)

help :: IO Flag
help = do
    let header = "Usage: rose [FILES...] [OPTIONS...]"
    putStrLn $! usageInfo header options
    exitSuccess

buildDir :: FilePath -> IO Flag
buildDir path = do
    dir <- makeAbsolute path
    return $! BuildDir (dir ++ "/")

options :: [OptDescr (IO Flag)]
options = id $! [
        Option "h" ["help"]              (NoArg help)
            "Displays help information",
        Option "v" ["verbosity"]         (OptArg verbosity "LEVEL")
            "Controls the amount and detail of messages",
        Option "s" ["silent"]            (NoArg silent)
            "No output (same as -v0)",
        Option ""  ["trace"]             (NoArg (return Trace))
            "Lowest-level debug info. Not neeeded for end users",
        Option ""  ["debug-info"]        (NoArg debug_info)
            "Enables lower-level debug info",
        Option "B" ["build-dir"]         (ReqArg buildDir "DIRECTORY")
            "Directory to put build files",
        Option ""  ["threaded"]          (NoArg (return Threaded))
            "Turns on multi-threaded building"
    ] ++ (fmap (pure . Warn) <$> warningOptions)
      ++ (fmap (pure . Flag) <$> flagOptions)

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

getCmdLine :: IO CmdLine
getCmdLine = do
    args <- reverse <$> getArgs
    let (opts, nons, errs) = getOpt RequireOrder options args
    mkCmdLine (reverse <$> sequence opts) nons errs
