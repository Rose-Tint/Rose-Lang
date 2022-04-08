{-# LANGUAGE BangPatterns #-}

module Build where

import Prelude hiding (readFile, lines)

import Control.Monad ((<$!>), when, forM_, mapM_)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import Data.Text.IO (readFile)
import System.Directory
import System.IO ()
import Text.Parsec (parse)

import CmdLine (CmdLine(..))
import CmdLine.Flags
import Analyzer.Analyzer
import Analyzer.Error (prettyError)
import Builder.Builder
import Builder.CmdLine
import Builder.Output
import Parser.Data (Expr, Import(..))
import Parser.Error (prettyParseErr)
import Parser.Parser
import Typing.Checker
import Pretty
import Utils


default (Int, Double)



build :: BuilderIO ()
build = do
    files <- getSourceFiles
    forM_ files buildFile
    status "Finished building\n"


buildFile :: FilePath -> BuilderIO ()
buildFile path = do
    setFilePath path
    name <- getModule
    setBuildDir (modToPath name)
    message ("Building Module ["+|name|+"]\n")
    dir <- getBuildDir
    doTrace <- cmdTrace <$!> getCmdLine
    when doTrace <#>
        createDirectoryIfMissing True dir

    src <- readFile <#> path

    (_, src') <- getImports src

    parseRes <- parseFile src src'
    analyzeFile src parseRes
    return ()

-- get the list of imports to build
getImports :: Text -> BuilderIO ([Import], Text)
getImports src = do
    name <- getModule
    case parse importsParser name src of
        Left err -> fatal
            "%s\n$rFailed while parsing module\n"
            [prettyParseErr err src]
        Right (modName, imports, src') -> do
            when (name /= modName) $ fatal
                "module declaration does not match \
                \file-name \n\
                \    Expected: %s\n\
                \    Found   : %s\n\
                \$rFailed while parsing module %s\n"
                [name, modName, name]
            return (imports, src')

parseFile :: Text -> Text -> BuilderIO [Expr]
parseFile src src' = do
    name <- getModule
    debug ("Parsing   ["+|name|+"]\n")
    case parse roseParser name src of
        Left err -> fatal
            (prettyParseErr err src|+
            "\nFailed while parsing module("+|name|+")\n")
        Right exprs -> do
            trace "Parse-Tree.txt" $
                concatMap pretty exprs
            return exprs


analyzeFile :: Text -> [Expr] -> BuilderIO Analysis
analyzeFile src es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    let !res = analyze_ $! mapM_ infer_ es
    trace "Symbol-Table.txt" $
        detailed (arTable res)
    if null $ arErrors res then
        return res
    else let lns = T.lines src in do
        forM_ (arErrors res) $ \em -> do
            message (prettyError lns em)
        flgs <- cmdFlags <$!> getCmdLine
        if f_fatal_errors `isFEnabled` flgs then fatal
            ("Failed while analyzing module ("+|name|+")\n")
        else
            return res
