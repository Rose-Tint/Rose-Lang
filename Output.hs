{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-
TODO:
    implement verbosity
-}

module Output where

import System.Environment

import Data.Maybe
import Text.Read hiding (reset)
import Text.Printf
import Text.Parsec
import Text.Parsec.Error

import Data.List
import Data.Set hiding (filter, foldl')

import Color



data CmdLine = CmdLine {
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


-- foldr  :: (Arg -> Str -> Str)
-- foldl  :: (Str -> Arg -> Str)
-- printf :: Str -> Arg -> String

success, fatal, warn, message, debug ::
    Int -> String -> [String] -> IO ()
success = myPutStr Green 2
fatal = myPutStr Red 2
warn = myPutStr Yellow 2
message = myPutStr Reset 2
status = myPutStr Reset 3
debug = myPutStr Cyan 4


myPutStr ::
    Color -> Int -> Int -> String -> [String] -> IO ()
myPutStr clr thresh vrb fStr args =
    if vrb >= thresh then
        putStr $ foldl' printf fStr args `colored` clr
    else
        return ()


printSrcLine :: Int -> Int -> String -> IO ()
printSrcLine v ln line = message v
    -- I know that you think the printf here is redundant.
    -- I do too. But I assure you: it is needed.
    (printf "%4d | %s\n" ln (reset line)) []


printParseErrHeader :: Int -> SourcePos -> IO ()
printParseErrHeader v pos = fatal v
        "Error while parsing %s:\n"
        [show pos]


printParseErrMsg :: Int -> Int -> [Message] -> IO ()
printParseErrMsg v col msgs = do
    -- add 7 to col to account for line number and pipe char
    _ <- message v (replicate (col + 6) ' ') []
    _ <- message v ("^" `colored` Red) []
    printParseErrMsg' v msgs


printParseErrMsg' :: Int -> [Message] -> IO ()
printParseErrMsg' v msgs = message v msgs' []
    where
        msgs' = showErrorMessages "or"
            "unknown parse error"
            "but expected:"
            "found:"
            "end of input"
            msgs


printParseErr :: Int -> ParseError -> String -> IO ()
printParseErr v err input = do
    if errLn < length srcLines then do
        _ <- printParseErrHeader v (errorPos err)
        _ <- printSrcLine v (errLn + 1) (srcLines!!errLn)
        _ <- printParseErrMsg v (sourceColumn src) errMsgs
        message v "\n" []
    else do
        _ <- fatal v "unexpected EOF resulting from:" []
        _ <- printParseErrMsg' v errMsgs
        message v "\n" []
    where
        src = errorPos err
        srcLines = lines input
        errLn = sourceLine src - 1
        errMsgs = errorMessages err
