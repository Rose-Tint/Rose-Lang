module Build (
    build,
) where

import Prelude hiding (readFile)

import Control.Monad ((<$!>), when, forM_)
import qualified Data.Text as T (lines)
import Data.Text.IO (readFile)
import System.Directory
import Text.Parsec (parse)

import CmdLine.Flags
import Common.Var
import Analyzer.Analyzer (Analysis(..), analyze_)
import Analyzer.Checker (infer_)
import Analyzer.Error (prettyError)
import Builder.Builder
import Builder.CmdLine
import Builder.Output
import Parser
import Parser.Data (Expr)
import Parser.Error (prettyParseErr)
import Pretty
import Utils (modToPath)


default (Int, Double)


build :: BuilderIO ()
build = do
    files <- getSourceFiles
    forM_ files $ buildFile
    status "Finished building\n"

buildFile :: FilePath -> BuilderIO ()
buildFile path = do
    setFilePath path
    name <- getModule
    isUTD <- isUpToDate name
    if isUTD then return () else do
        setBuildDir (modToPath name)
        message ("Building Module ["*|name|*"]\n")
        dir <- getBuildDir
        doTrace <- cmdTrace <$!> getCmdLine
        when doTrace <#>
            createDirectoryIfMissing True dir
        src <- readFile <#> path
        setSource src
        ParseResults imports tree _ <- parseFile
        forM_ imports $ \(Import (Var modName _) _ _ _) -> 
            buildFile (modToPath modName)
        analyzeFile tree
        addUTDModule name

parseFile :: BuilderIO ParseResults
parseFile = do
    name <- getModule
    src <- getSource
    debug ("Parsing   ["+|name|+"]\n")
    case parse parser name src of
        Left err -> do
            src' <- getSource
            fatal $ prettyParseErr err src'
                |+"\nFailed while parsing module("+|name|+")"
        Right res -> do
            trace "Parse-Tree.txt" $
                concatMap pretty (prParseTree res)
            return res

analyzeFile :: [Expr] -> BuilderIO Analysis
analyzeFile es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    let !res = analyze_ $! mapM_ infer_ es
    trace "Symbol-Table.txt" $
        detailed (arTable res)
    if null $ arErrors res then
        return res
    else do
        lns <- T.lines <$> getSource
        forM_ (arErrors res) $ \em -> do
            message (prettyError lns em)
        flgs <- cmdFlags <$!> getCmdLine
        if f_fatal_errors `isFEnabled` flgs then fatal
            ("Failed while analyzing module ("+|
                name|+")\n")
        else
            return res
