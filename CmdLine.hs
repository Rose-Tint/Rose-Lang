module CmdLine (
    CmdLine(..),
    getCmdLine
) where

import System.Console.GetOpt
import System.Directory
    (makeAbsolute, getCurrentDirectory)
import System.IO (FilePath)



data CmdLine
    = CmdLine {
        cmdFiles :: [String],
        cmdVerb :: Int,
        cmdBuildDir :: FilePath,
        cmdCurrDir :: FilePath,
        cmdTrace :: Bool,
        cmdErrors :: [String]
    }


data Flag
    = Verbosity Int
    | BuildDir FilePath
    | Trace
    | NoOp



verbosity :: Maybe String -> IO Flag
verbosity Nothing = return (Verbosity 3)
verbosity (Just str) = return (Verbosity (read str))

silent :: IO Flag
silent = return (Verbosity 0)

help :: IO Flag
help = do
    let header = "Usage: rose [FILES...] [OPTIONS...]"
    _ <- putStrLn $! usageInfo header options
    return NoOp

buildDir :: FilePath -> IO Flag
buildDir path = do
    dir <- makeAbsolute path
    return (BuildDir dir)


options :: [OptDescr (IO Flag)]
options = [
        Option "h" ["help"]      (NoArg help)
            "Displays help information",
        Option "v" ["verbosity"] (OptArg verbosity "LEVEL")
            "Controls the amount and detail of messages to stderr",
        Option "s" ["silent"]    (NoArg silent)
            "No output (same as -v0)",
        Option "t" ["trace"]     (NoArg (return Trace))
            "Enables tracing to an optional file, or stdout otherwise",
        Option "B" ["build-dir"] (ReqArg buildDir "DIRECTORY")
            "Directory to put build files"
    ]


setFlags :: [Flag] -> CmdLine -> CmdLine
setFlags [] cmd = cmd
setFlags (flg:flgs) cmd = setFlags flgs $! case flg of
    Verbosity v -> cmd { cmdVerb = v }
    BuildDir dir -> cmd { cmdBuildDir = dir ++ "/" }
    Trace -> cmd { cmdTrace = True }
    NoOp -> cmd


mkCmdLine :: IO [Flag] -> [String] -> [String] -> IO CmdLine
mkCmdLine flgs fnames errs = do
    flgs' <- flgs
    currDir <- getCurrentDirectory
    defBuildDir <- makeAbsolute "./Rose-Build"
    let cmd = CmdLine {
            cmdFiles = fnames,
            cmdVerb = 2,
            cmdErrors = errs,
            cmdTrace = False,
            cmdBuildDir = defBuildDir,
            cmdCurrDir = currDir
        }
    return $! setFlags flgs' cmd


getCmdLine :: [String] -> IO CmdLine
getCmdLine args =
    let (opts, nons, errs) = getOpt RequireOrder options args
    in mkCmdLine (sequence opts) nons errs
