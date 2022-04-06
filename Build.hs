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
import Analyzer.Analyzer
import Analyzer.Error (prettyError)
import Builder.Builder
import Builder.CmdLine
import Builder.Output
import Parser.Data (Expr)
import Parser.Error (prettyParseErr)
import Parser.Parser (roseParser)
import Typing.Checker
import Pretty
-- import Threading
import Utils


default (Int, Double)



build :: BuilderIO ()
build = do
    files <- getSourceFiles
    forM_ files buildFile
    status "Finished Building All Modules\n" []


buildFile :: FilePath -> BuilderIO ()
buildFile path = do
    setFilePath path
    name <- getModule
    setBuildDir (modToPath name)
    message "Building Module [%s]\n" [name]
    dir <- getBuildDir
    doTrace <- cmdTrace <$!> getCmdLine
    when doTrace <#>
        createDirectoryIfMissing True dir

    src <- readFile <#> path

    parseRes <- parseFile src
    analyzeFile src parseRes
    return ()


parseFile :: Text -> BuilderIO [Expr]
parseFile src = do
    name <- getModule
    debug "Parsing   [%s]\n" [name]
    case parse roseParser name src of
        Left err -> fatal
            "%s\nFailed while parsing module (%s)\n"
            [prettyParseErr err src, name]
        Right exprs -> do
            trace "Parse-Tree.txt" $
                concatMap pretty exprs
            return exprs


analyzeFile :: Text -> [Expr] -> BuilderIO (Analysis ())
analyzeFile src es = do
    name <- getModule
    debug "Analyzing [%s]\n" [name]
    let !res = analyze $! mapM_ infer_ es
    trace "Symbol-Table.txt" $
        detailed (arTable res)
    if null $ arErrors res then
        return res
    else let lns = T.lines src in do
        forM_ (arErrors res) $ \em -> do
            message (prettyError {-cmd-} lns em) []
        return res
        -- fatal (cmdVerb cmd)
        --     "Failed while analyzing module (%s)\n"
        --     [name]
