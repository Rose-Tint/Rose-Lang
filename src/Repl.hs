module Repl (repl) where

import Data.List (stripPrefix, isPrefixOf)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)

import AST.Value
import Data.Table (emptyTable)
import Parser.Parser (runAlex, replP)
import Typing.Infer
import Typing.Inferable
import Text.Pretty
import Utils.FilePath (modToPath)


data Cmd
    = TypeOf !Value
    | Load [FilePath]
    | Quit
    | Help
    | Error String

prompt :: IO String
prompt = do
    putStr " ~>"
    hFlush stdout
    str <- getLine
    case stripPrefix ":{" str of
        Nothing -> return str
        Just suffix -> do
            rest <- promptMulti
            return $! suffix ++ rest

promptMulti :: IO String
promptMulti = do
    putStr " ->"
    hFlush stdout
    str <- getLine
    if ":}" `isPrefixOf` str then
        return ""
    else do
        rest <- promptMulti
        return $! str ++ rest

readCmd :: String -> Cmd
readCmd ('t':rest) = case runAlex rest replP of
    Left err -> Error err
    Right val -> TypeOf val
readCmd ('l':rest) = Load (words rest)
readCmd ('m':rest) = Load (modToPath <$> words rest)
readCmd ('q':_) = Quit
readCmd ('h':_) = Help
readCmd [] = Error "expected command"
readCmd str = Error ("unknown command: '"+|str|+"'")

evalInput :: String -> IO ()
evalInput [] = return ()
evalInput (':':rest) = case readCmd rest of
    Load _ -> error "'load' not yet implemented"
    Quit -> do
        putStrLn "Thanks for playing!"
        exitSuccess
    TypeOf val -> case runInfer emptyTable (infer val) of
        Left err -> error (pretty err)
        Right (typ, _cons, _tbl) -> putStrLn (pretty typ)
    Help -> putStrLn "\
\Usage: [COMMAND] [EXPRESSION]\n\
\Commands:\n\
\    :q          Exits REPL\n\
\    :h          Displays help information\n\
\    :t          Displays the type of the expression\n\
\    :l          Loads data from filepaths\n\
\    :m          Loads data from modules\n"
    Error str -> error str
evalInput _ = error "evaluation not yet implemented"

repl :: IO ()
repl = prompt >>= evalInput >> repl
