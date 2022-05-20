{-# LANGUAGE LambdaCase #-}

module Repl (repl) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.List (stripPrefix, isPrefixOf)
import System.IO
import System.Exit (exitSuccess)

import AST.Value
import Data.Table (emptyTable)
import Parser.Parser (runAlex, replP)
import Typing.Inferable
import Text.Pretty
import Utils.FilePath (modToPath)


data Cmd
    = TypeOf !Value
    | Load [FilePath]
    | Quit
    | Help

type Repl = ExceptT String IO


prependLines :: String -> String -> String
prependLines pre = unlines . fmap (pre++) . lines

print' :: Pretty a => a -> Repl ()
print' a = do
    -- prepends " => " to each line
    let str = prependLines "= | " (pretty a)
    lift (putStr (uncolor (processString str)))

prompt :: Repl String
prompt = do
    lift $ putStr "? | "
    lift $ hFlush stdout
    str <- lift getLine
    case stripPrefix ":{" str of
        Nothing -> return str
        Just suffix -> do
            rest <- promptMulti
            return $! suffix ++ rest

promptMulti :: Repl String
promptMulti = do
    lift $ putStr "+ | "
    lift $ hFlush stdout
    str <- lift getLine
    if ":}" `isPrefixOf` str then
        return ""
    else do
        rest <- promptMulti
        return $! str ++ ('\n':rest)

readCmd :: String -> Repl Cmd
readCmd ('t':rest) = case runAlex rest replP of
    Left err -> throwE err
    Right val -> return (TypeOf val)
readCmd ('l':rest) = return (Load (words rest))
readCmd ('m':rest) = return (Load (modToPath <$> words rest))
readCmd ('q':_) = return Quit
readCmd ('h':_) = return Help
readCmd [] = throwE "expected command"
readCmd str = throwE ("unknown command: '"+|str|+"'")

eval :: String -> Repl ()
eval [] = return ()
eval (':':rest) = readCmd rest >>= \case
    Load _ -> throwE "'load' not yet implemented"
    Quit -> do
        print' "Thanks for playing!"
        lift exitSuccess
    TypeOf val -> case makeInference emptyTable (infer val) of
        Left err -> throwE ("<stdin>"+|(lines (tail rest),err))
        Right scheme -> print' scheme
    Help -> print' "\
\Usage: [COMMAND] [EXPRESSION]\n\
\Commands:\n\
\    :q          Exits REPL\n\
\    :h          Displays help information\n\
\    :t          Displays the type of the expression\n\
\    :l          Loads data from filepaths\n\
\    :m          Loads data from modules\n"
eval _ = throwE "evaluation not yet implemented"

evalInput :: Repl ()
evalInput = prompt >>= eval

repl :: IO ()
repl = do
    hSetEcho stderr False
    hSetEcho stdout False
    eith <- runExceptT evalInput
    case eith of
        Left err -> putStr $ uncolor $ processString $
            prependLines "! | " (pretty err)
        Right _ -> return ()
    repl
