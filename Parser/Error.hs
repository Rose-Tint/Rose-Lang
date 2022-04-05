module Parser.Error (printParseErr) where

import Prelude hiding (lines)

import Data.Text (Text, unpack, lines)
import Text.Parsec (SourcePos, sourceColumn, sourceLine)
import Text.Parsec.Error

import Color
import Output


default (Int, Double)



printSrcLine :: Int -> Int -> Text -> IO ()
printSrcLine v ln line = message v
    (show ln ++ " | " ++ unpack line ++ "\n") []


printParseErrHeader :: Int -> SourcePos -> IO ()
printParseErrHeader v pos = fatal v
        "Error while parsing %s:\n" [show pos]


printParseErrMsg :: Int -> Int -> [Message] -> IO ()
printParseErrMsg v col msgs = do
    -- add 7 to col to account for line number and pipe char
    message v (replicate (col + 6) ' ') []
    message v ("^" `colored` Red) []
    printParseErrMsg' v msgs


printParseErrMsg' :: Int -> [Message] -> IO ()
printParseErrMsg' v msgs = message v msgs' []
    where
        msgs' = showErrorMessages "or"
            "unknown parse error"
            "Expected:"
            "Found:"
            "end of input"
            msgs


printParseErr :: Int -> ParseError -> Text -> IO ()
printParseErr v err input = do
    if errLn < length srcLines then do
        printParseErrHeader v (errorPos err)
        printSrcLine v (errLn + 1) (srcLines!!errLn)
        printParseErrMsg v (sourceColumn src) errMsgs
        message v "\n" []
    else do
        fatal v "unexpected EOF resulting from:" []
        printParseErrMsg' v errMsgs
        message v "\n" []
    where
        src = errorPos err
        srcLines = lines input
        errLn = sourceLine src - 1
        errMsgs = errorMessages err


