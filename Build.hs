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
import Parser.Error (printParseErr)
import Parser.Parser (roseParser)
import Typing.Checker
import Pretty
-- import Threading
import Utils


default (Int, Double)



build :: BuilderIO ()
build = do
    files <- getSourceFiles
    forM_ files $ \file -> do
        setFilePath file
        buildFile
    status "Finished Building All Modules\n" []


buildFile :: BuilderIO ()
buildFile = do
    name <- getModule
    message "Building Module [%s]\n" [name]
    dir <- getBuildDir
    doTrace <- cmdTrace <$!> getCmdLine
    when doTrace <#>
        createDirectoryIfMissing True dir

    path <- getFilePath
    src <- readFile <#> path

    parseRes <- parseFile src
    analyzeFile src parseRes
    return ()


parseFile :: Text -> BuilderIO [Expr]
parseFile src = do
    name <- getModule
    info "Parsing   [%s]\n" [name]
    case parse roseParser name src of
        Left err -> do
            printParseErr err src
            fatal "Failed while parsing module (%s)\n" [name]
        Right exprs -> do
            trace (modToDir name ++ "Parse-Tree.txt") $
                concatMap pretty exprs
            return exprs


analyzeFile :: Text -> [Expr] -> BuilderIO (Analysis ())
analyzeFile src es = do
    cmd <- getCmdLine
    name <- getModule
    info "Analyzing [%s]\n" [name]
    let res = analyze cmd (mapM_ infer_ es)
    trace (modToDir name ++ "Symbol-Table.txt") $
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
