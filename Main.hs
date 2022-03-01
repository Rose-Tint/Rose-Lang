module Main where

import Data.Set (toList)
import Data.Time
import Text.Parsec

import CmdLine
import Output
import Parser.Parser
import Parser.Pretty


parseFiles :: Int -> [String] -> IO Bool
parseFiles _ [] = return True
parseFiles v (file:files) = do
    suc <- parseFile v file
    if suc then
        parseFiles v files
    else
        return False
    

parseFile :: Int -> String -> IO Bool
parseFile v fname = do
    _ <- status v "Parsing [%s]\n" [fname]
    input <- readFile fname
    case parse thornP fname input of
        Left err    -> do
            _ <- printParseErr v err input
            return False
        Right exprs -> do
            let exprStrs = fmap prettyExpr exprs
            _ <- trace v "AST results:\n%s\n"
                [concat exprStrs]
            return True


main :: IO ()
main = do
    cmd <- cmdLine
    let verb = cmdVerb cmd
        files = toList $ cmdFiles cmd
    _ <- debug verb
        "Parsing the following files in order:\n%s\n"
        [show files]
    parseStart <- getCurrentTime
    _ <- debug verb "Began parsing at %s\n" [show parseStart]
    err <- parseFiles verb files
    parseEnd <- getCurrentTime
    _ <- debug verb "Finished parsing at %s\n" [show parseEnd]
    _ <- (if err then
        success verb "Finished parsing successfully\n" []
    else
        fatal verb "Finished parsing with an error\n" [])
    _ <- debug verb "Finished in %s\n"
        [show (diffUTCTime parseEnd parseStart)]
    return ()
