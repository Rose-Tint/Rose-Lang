module Build where

import Prelude hiding (readFile, lines)

import Control.Monad (when, forM_, foldM_)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import Data.Text.IO (readFile)
import System.Directory
import System.IO ()
import Text.Parsec (parse)

import CmdLine (CmdLine(..))
import Analyzer.Analyzer
import Analyzer.Error (prettyError)
import Parser.Data (Expr)
import Parser.Error (printParseErr)
import Parser.Parser (roseParser)
import SymbolTable
import SymbolTable.Trie (prettyTrie)
import Typing.Checker
import Output
import Pretty
import Threading
import Utils


default (Int, Double)


type ModuleName = String


type BuildResult = ()



build :: CmdLine -> IO BuildResult
build cmd = do
    if cmdThreaded cmd then do
        mgr <- newManager
        forM_ (cmdFiles cmd) $
            fork mgr .! buildFile cmd
        waitAll mgr
    else
        forM_ (cmdFiles cmd) $ buildFile cmd
    status (cmdVerb cmd)
        "Finished Building All Modules\n" []
    return ()


buildFile :: CmdLine -> FilePath -> IO BuildResult
buildFile cmd relPath = do
    let modName = pathToModule relPath
        verb = cmdVerb cmd
        buildDir = cmdBuildDir cmd ++
            pathToDir relPath

    when (cmdTrace cmd) $
        createDirectoryIfMissing True buildDir
    message verb "Building Module [%s]\n" [modName]

    src <- makeAbsolute relPath >>= readFile

    parseRes <- parseFile cmd src modName
    analyzeFile cmd src modName parseRes
    return ()


parseFile :: CmdLine -> Text -> ModuleName -> IO [Expr]
parseFile cmd src name = do
    info (cmdVerb cmd) "Parsing   [%s]\n" [name]
    let buildDir = cmdBuildDir cmd ++ modToDir name
    case parse roseParser name src of
        Left err -> do
            printParseErr (cmdVerb cmd) err src
            fatal (cmdVerb cmd)
                "Failed while parsing module (%s)\n" [name]
        Right exprs -> do
            trace cmd (buildDir ++ "Parse-Tree.txt") $
                concatMap pretty exprs
            return exprs


analyzeFile :: CmdLine -> Text -> ModuleName -> [Expr] -> IO (Analysis ())
analyzeFile cmd src name es = do
    info (cmdVerb cmd) "Analyzing [%s]\n" [name]
    let buildDir = cmdBuildDir cmd ++ modToDir name
        res = analyze cmd $! foldM_ (\_->infer_) () es
    trace cmd (buildDir ++ "Symbol-Table.txt") $!
        detailed (arTable res) ++
        "\n\n\n\n" ++ prettyTrie (tblGlobals $ arTable res)
    if null $! arErrors res then
        return res
    else let lns = T.lines src in do
        mapM (putStrLn . prettyError cmd lns) (arErrors res)
        putStrLn ""
        return res
        -- fatal (cmdVerb cmd)
        --     "Failed while analyzing module (%s)\n"
        --     [name]
