module Build where

import Prelude hiding (readFile, lines)

import Control.Monad (foldM_, unless)
import Data.Text (Text, lines)
import Data.Text.IO (readFile)
-- import Data.Time (diffUTCTime, getCurrentTime)
import System.Directory
import System.IO ()
import Text.Parsec (parse)

import CmdLine (CmdLine(..))
import Parser.Data (Expr)
import Parser.Error (printParseErr)
import Parser.Parser (roseParser)
import Parser.Pretty (prettyExpr)
import Semantics.Analysis
import Semantics.Error (Error(UnknownError), prettyError)
import Semantics.SymbolTable
import Semantics.Visitor
import Output
import Utils (pathToModule)


type ModuleName = String


type BuildResult = ()



modPathToRelDir :: FilePath -> FilePath
modPathToRelDir [] = "/"
modPathToRelDir ".th" = "/"
modPathToRelDir (c:cs) = (c:modPathToRelDir cs)


buildFile :: CmdLine -> FilePath -> IO BuildResult
buildFile cmd relPath = do
    let modName = pathToModule relPath
    let verb = cmdVerb cmd
    let buildDir = (cmdBuildDir cmd) ++ "/" ++
            modPathToRelDir relPath
    message verb "Building Module [%s]\n" [modName]
    debug verb "module build dir: %s\n" [buildDir]
    src <- makeAbsolute relPath >>= readFile
    parseRes <- parseFile cmd src modName
    trace cmd (buildDir ++ "Abstract-Syntax-Tree.txt")
        (concat $ fmap prettyExpr parseRes)
    symTbl <- analyzeFile cmd src modName parseRes
    trace cmd (buildDir ++ "Symbol-Table.txt")
        (prettySymbolTable symTbl)
    status verb "Finished Building [%s]\n" [modName]


parseFile :: CmdLine -> Text -> ModuleName -> IO [Expr]
parseFile cmd src name = do
    info (cmdVerb cmd) "Parsing [%s]\n" [name]
    case parse roseParser name src of
        Left err -> do
            printParseErr (cmdVerb cmd) err src
            fatal (cmdVerb cmd)
                "Failed while parsing module (%s)\n" []
        Right exprs -> return exprs


analyzeFile :: CmdLine -> Text -> ModuleName -> [Expr]
            -> IO SymbolTable
analyzeFile cmd src name exprs = do
    info (cmdVerb cmd) "Analyzing [%s]\n" [name]
    case visit name visitor of
        Okay _ st err -> do
            unless (err == UnknownError)
                (message v (prettyError srcLines name err) []
                >> return ())
            return st
        Error _ err ->
            fatal v (prettyError srcLines name err) []
    where
        v = cmdVerb cmd
        srcLines = lines src
        visitor = do
            preprocess exprs
            foldM_ (\_ expr ->
                analyzeExpr expr
                >> return ())
                () exprs

