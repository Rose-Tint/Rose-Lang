module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Builder
import Cmd
import Compiler (compile)
import Repl


build :: Builder ()
build = compile--do
--     timeStart <- io getCurrentTime
--     files <- asks cmdFiles
--     mapM_ buildFile files
--     timeEnd <- io getCurrentTime
--     let diff = diffUTCTime timeEnd timeStart
--     message ("Finished in "+|diff|+"\n")

-- buildFile :: FilePath -> Builder Table
-- buildFile [] = return emptyTable
-- buildFile path = do
--     skip <- hasBeenVisited path
--     if skip then do
--         return emptyTable
--     else do
--         bReadFile path
--         name <- gets moduleName
--         message ("Building Module ["+|name|+"]\n")
--         ParseTree imports exprs <- parseFile
--         tbl <- loadImports imports
--         tbl' <- runAnalysis tbl exprs
--         mapM_ (buildFile.Utils.modToPath.varName) imports
--         finalizeVisit
--         return tbl'

-- loadImports :: [Import] -> Builder Table
-- loadImports [] = return emptyTable
-- loadImports (Var str _:imports) = do
--     let name = strToMod str
--     mTable <- loadObjectFile name
--     case mTable of
--         Nothing -> loadImports imports
--         Just tbl1 -> do
--             tbl2 <- loadImports imports
--             return $! (unionTable tbl1 tbl2)

main :: IO ()
main = do
    (task, errs) <- readCmdLine
    unless (null errs) $ do
        mapM_ putStrLn errs
        exitFailure
    case task of
        Repl -> repl
        Build cmd -> runBuilder build cmd
