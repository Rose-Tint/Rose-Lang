{-# LANGUAGE LambdaCase #-}

module Compiler (
    compile,
    FileData(..),
    ModuleData(..),
) where

import Prelude hiding (readFile)

import Control.Monad (forM, forM_, unless)
import qualified Data.ByteString.Lazy.Char8 as BS

import Analysis.Error
import AST (ParseTree(..), Expr)
import Builder
import Cmd
import Common.Module
import Data.DepGraph
import Data.Table
import Parser.Parser (runAlex, rose)
import Text.Pretty
import Typing.Inferable


-- type Stream = LazyByteString

data FileData = FileData {
    fdName :: ModName,
    fdImports :: [Import],
    fdExprs :: [Expr]
    }

data ModuleData = ModData {
    mdName :: ModName,
    mdTable :: Table,
    mdExprs :: [Expr]
    }


compile :: [FilePath] -> Builder ()
compile files = do
    files' <- mapM readFile files
    sched <- makeSchedule files'
    mapM_ compileModule sched

-- only needs the IO portion of `Builder`
readFile :: FilePath -> Builder FileData
readFile path = do
    let name = pathToMod path
    debug ("Parsing ["+|name|+"]\n")
    base <- gets sourceDirectory
    src <- io (BS.readFile (base ++ path))
    modify $ \s -> s {
        moduleName = name,
        sourceCode = src
        }
    ParseTree imps exprs <- case runAlex src rose of
        Left msg -> fatal (name|+|msg|+"\n")
        Right tree -> do
            traceFile "tree" tree
            return tree
    return (FileData name imps exprs)

-- | Sorts the file data such that for any module,
-- every imported modules comes before it in the list.
--
-- (in other words, ensures thatimports precede their
-- importee)
makeSchedule :: [FileData] -> Builder [FileData]
makeSchedule [] = return []
makeSchedule files = case scheduleDepGraph edges of
    Depends files' -> do
        debug $ "In order, the following will be built:\n"+|
            indentCatLns (fdName <$> files')
        return files'
    Circular files' -> fatal $
        "Imports for a circular dependency:\n    "
        +|", which imports "`seps`fmap fdName files'
    where
        edges = fmap (\fd@(FileData nm is _) ->
            (fd, pretty nm, pretty <$> is)
            ) files

-- | Gets the required data such as object files
resolveDeps :: FileData -> Builder ModuleData
resolveDeps (FileData name imports exprs) = do
    modify $ \s -> s { moduleName = name }
    unless (null imports) $
        debug ("Resolving dependencies for ["+|name|+"]\n")
    tables <- forM imports $ \imp ->
        loadObjectFile imp >>= \case
            Nothing -> fatal $
                "Could not find object file for "+|imp|+
                ".\n    Possible cause: faulty schedule"
            Just tbl -> do
                return tbl
    let table = foldr unionTable emptyTable tables
    return (ModData name table exprs)

compileModule :: FileData -> Builder ()
compileModule fd = do
    (ModData name tbl exprs) <- resolveDeps fd
    status ("Building module ["+|name|+"]\n")
    let (tbl', errs) = inferTopLevel tbl exprs
    traceFile "table" tbl'
    printAnalysisErrors name errs
    writeBin ".o" tbl'
    return ()

printAnalysisErrors :: ModName -> [ErrMsg] -> Builder ()
printAnalysisErrors _ [] = return ()
printAnalysisErrors name errs = do
    lns <- gets (BS.lines . sourceCode)
    forM_ errs $ \err -> do
        let msg = "\n"+|name|+|ErrMsg lns err|+"\n"
        (fatalErrors . flags) ??> fatal msg
        message msg
