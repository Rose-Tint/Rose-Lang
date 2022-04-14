module Build (
    ImportInfo(..),
    build,
) where

import Prelude hiding (readFile)

import Control.Monad ((<$!>), when, forM_)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import Data.Text.IO (readFile)
import System.Directory
import Text.Parsec (parse, SourcePos)

import CmdLine.Flags
import Analyzer.Analyzer (Analysis(..), analyze_)
import Analyzer.Error (prettyError)
import Builder.Builder
import Builder.CmdLine
import Builder.Output
import Parser.Data (Expr, ImportModule(..))
import Parser.Error (prettyParseErr)
import Parser.Parser (importsParser, roseParser)
import Typing.Checker (infer_)
import Pretty
import Utils (modToPath)


default (Int, Double)


data ImportInfo = ImportInfo {
        iiModules :: [ImportModule],
        iiRemSource :: Text,
        iiEndPos :: !SourcePos
    }


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
        ImportInfo imports src' pos <- getImports src
        forM_ imports $ \imp ->
            buildFile (modToPath (impModule imp))
        setSource src
        parseRes <- parseFile pos src'
        analyzeFile parseRes
        addUTDModule name

-- get the list of imports to build
getImports :: Text -> BuilderIO ImportInfo
getImports src = do
    name <- getModule
    case parse importsParser name src of
        Left err -> fatal $
            prettyParseErr err src|+
            "\n$rFailed while parsing module\n"
        Right (modName, imports, src', pos) -> do
            when (name /= modName) $ fatal $
                "module declaration does not match the \
                \filename\n\
                \    Expected: "+|name|+"\n\
                \    Found   : "+|modName|+"\n$r\
                \Failed while parsing module "+|name|+"\n"
            return $ ImportInfo imports src' pos

parseFile :: SourcePos -> Text -> BuilderIO [Expr]
parseFile pos src = do
    name <- getModule
    debug ("Parsing   ["+|name|+"]\n")
    case parse (roseParser pos) name src of
        Left err -> do
            src' <- getSource
            fatal $ prettyParseErr err src' +\
                "Failed while parsing module("+|name|+")"
        Right exprs -> do
            trace "Parse-Tree.txt" $
                concatMap pretty exprs
            return exprs

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
