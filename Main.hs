module Main where

import Data.List
import Data.Set (toList)
import Text.Parsec

import CmdLine
import Output
import Parser.Parser
import Pretty


parseFiles :: Int -> [String] -> IO Int
parseFiles _ [] = return 0
parseFiles v (file:files) = do
    suc <- parseFile v file
    _ <- debug v "\n" []
    err_c <- parseFiles v files
    return $ fromEnum (not suc) + err_c
    

parseFile :: Int -> String -> IO Bool
parseFile v fname = do
    _ <- message v "Parsing [%s]\n" [fname]
    input <- readFile fname
    case parse thornP fname input of
        Left err    -> do
            _ <- printParseErr v err input
            return False
        Right exprs -> do
            let exprStrs = fmap prettyExpr exprs
            _ <- debug v "AST results:\n%s"
                [intercalate "\n" exprStrs]
            return True


main :: IO ()
main = do
    -- let files = ["examples.th"]
    cmd <- cmdLine
    let verb = cmdVerb cmd
    let files = toList $ cmdFiles cmd
    _ <- message verb
        "Parsing the following files in order:\n%s\n"
        [show files]
    err_c <- parseFiles verb files
    if err_c == 0 then
        success verb
            "Finished parsing successfully\n" []
    else if err_c == 1 then
        fatal verb
            "Finished parsing with 1 error\n" []
    else
        fatal verb
            "Finished parsing with %s errors\n"
            [show err_c]
