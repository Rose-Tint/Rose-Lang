module CmdLine where

import System.Environment

import Data.List
import Data.Maybe (fromMaybe)
import Data.Set hiding (filter, foldl')
import Text.Read (readMaybe)



data CmdLine
    = CmdLine {
        cmdVerb :: Int,
        cmdFlgs :: Set String,
        cmdFiles :: Set String
    }



cmdLine :: IO CmdLine
cmdLine = do
    args <- getArgs
    let flgs = filter ("-f" `isPrefixOf`) args
    let files = takeWhile (not . ("-" `isPrefixOf`)) args
    let verb = parseVerbosity $ find isVerbosityArg args
    return $ CmdLine verb (fromList flgs) (fromList files)
    where
        isVerbosityArg s = "-v" `isPrefixOf` s
                        || "--verbosity=" `isPrefixOf` s
        parseVerbosity Nothing = 3
        parseVerbosity (Just v) =
            case stripPrefix "--verbosity=" v of
                Nothing -> case stripPrefix "-v" v of
                    Nothing -> 3
                    Just v'  -> fromMaybe 4 (readMaybe v')
                Just v' -> fromMaybe 3 (readMaybe v')
