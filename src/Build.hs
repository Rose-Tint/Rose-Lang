module Build (
    build,
) where

import Prelude hiding (readFile)

import Control.Monad ((<$!>), when, forM_)
import Data.Text (unpack)
import qualified Data.Text as T (lines)
import Data.Text.IO (readFile)
import System.Directory

import CmdLine.Flags
import Common.Var
import Analyzer.Analyzer (Analysis(..), analyze_)
import Analyzer.Checker (infer_)
import Analyzer.Error (prettyError)
import Builder.Builder
import Builder.CmdLine
import Builder.Output
import Parser
import Pretty
import Utils (modToPath)


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
        Module imports tree <- parseFile
        forM_ imports $ \(Import (Var modName _) _) ->
            buildFile (modToPath modName)
        analyzeFile tree
        addUTDModule name

parseFile :: BuilderIO Module
parseFile = do
    name <- getModule
    src <- getSource
    debug ("Parsing   ["+|name|+"]\n")
    case parse (unpack src) of
        Left msg -> fatal $
            Red|+|name|+|msg|+"\n"+|Red|+
            "Failed while parsing module '"
            +|name|+"'\n"
        Right parseTree -> do
            trace "Parse-Tree.txt" $
                pretty parseTree
            return parseTree

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
