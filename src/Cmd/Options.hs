module Cmd.Options (
    optionOptions,
) where

import System.Console.GetOpt
import System.Directory
import System.Exit (exitSuccess)


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
    putStrLn $! usageInfo header optionOptions
    exitSuccess

buildDir :: FilePath -> IO Flag
buildDir path = do
    dir <- makeAbsolute path
    return $! BuildDir (dir ++ "/")

optionOptions :: [OptDescr (IO Flag)]
optionOptions = [
        Option "v" ["verbosity"]         (OptArg verbosity "LEVEL")
            "Controls the amount and detail of messages",
        Option "s" ["silent"]            (NoArg silent)
            "No output (same as -v0)",
        Option "T"  ["trace"]            (NoArg (return Trace))
            "Lowest-level debug info. Not neeeded for end users",
        Option ""  ["debug-info"]        (NoArg debug_info)
            "Enables lower-level debug info",
        Option "B" ["build-dir"]         (ReqArg buildDir "DIRECTORY")
            "Directory to put build files",
        Option ""  ["threaded"]          (NoArg (return Threaded))
            "Turns on multi-threaded building"
    ]
