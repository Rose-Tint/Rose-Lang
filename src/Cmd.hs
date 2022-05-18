module Cmd (
    module Cmd.Flags,
    module Cmd.Warnings,
    Task(..),
    CmdLine(..),
    readCmdLine,
) where

import Control.Monad (foldM)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Cmd.Flags
import Cmd.Warnings
import Utils.String (mReadInt)


default (Int, Double)


data Task
    = Build CmdLine
    | Repl

data CmdOpt
    = Verbose (Maybe String)
    | BuildDir FilePath
    | Trace
    | Threaded
    | Help
    | Flag (Flags -> Flags)
    | Warning (Warnings -> Warnings)

-- everything is strict for concurrency safety
data CmdLine = CmdLine {
    cmdFiles :: [String],
    verbosity :: Int,
    baseBuildDir :: FilePath,
    cmdTrace :: Bool,
    warnings :: Warnings,
    flags :: Flags,
    threaded :: Bool
    }


options :: [OptDescr CmdOpt]
options = [
        Option "v" ["verbose"]
            (OptArg Verbose "LEVEL")
            "Controls the amount and detail of messages",
        Option "T"  ["trace"]
            (NoArg Trace)
            "Low-level debug info. Not neeeded for end users",
        Option "B" ["build-dir"]
            (ReqArg BuildDir "DIRECTORY")
            "Directory to put build files",
        Option ""  ["threaded"]
            (NoArg Threaded)
            "Turns on multi-threaded building",
        Option "h" ["help"]
            (NoArg Help)
            "Displays help information"
    ]
    ++ (fmap Warning <$> warningOptions)
    ++ (fmap Flag <$> flagOptions)

defaultCmd :: CmdLine
defaultCmd = CmdLine {
    cmdFiles = [],
    verbosity = 1,
    baseBuildDir = "Rose-Build/",
    cmdTrace = False,
    warnings = defaultWarnings,
    flags = defaultFlags,
    threaded = False
    }

readCmdOpts :: CmdLine -> [CmdOpt] -> IO CmdLine
readCmdOpts = foldM (\cmd opt -> case opt of
        Verbose Nothing -> return $ cmd
            { verbosity = 2 }
        Verbose (Just str) -> case mReadInt 10 str of
            Nothing -> error
                "error reading verbosity level"
            Just n -> return $ cmd { verbosity = n }
        BuildDir dir -> do
            dir' <- makeAbsolute dir
            return $ cmd { baseBuildDir = (dir' ++ "/") }
        Trace -> return $ cmd { cmdTrace = True }
        Threaded -> return $ cmd { cmdTrace = True }
        Help -> do
            putStrLn $! usageInfo header options
            exitSuccess
        Flag f -> return $ cmd { flags = f (flags cmd) }
        Warning f -> return $ cmd
            { warnings = f (warnings cmd) }
        )
    where
        header = "Usage: rose [FILES...] [OPTIONS...]"

-- the input strings get reversed because `getOpt`
-- expects options first, but we want files first.
getOptions :: [String] -> ([CmdOpt], [String], [String])
getOptions = getOpt RequireOrder options . reverse

-- | Reads the command line, and it returns a tuple.
-- if the first value in the tuple is `Nothing`, then
-- that means a REPL is to be run.
--
-- TODO: find a better way to signal for the repl.
readCmdLine :: IO (Task, [String])
readCmdLine = do
    args <- getArgs
    case args of
        ("repl":args') ->
            let errs = ("args not allowed: "++) . show <$> args'
            in return (Repl, errs)
        _ -> do
            let (opts, nons, errs) = getOptions args
                    -- :: ([CmdOpt], [String], [String])
                cmd = defaultCmd {
                    -- reverse the files to preserve
                    -- their input order
                    cmdFiles = reverse nons 
                    }
            cmd' <- readCmdOpts cmd opts
            return (Build cmd', errs)
