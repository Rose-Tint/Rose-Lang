module Build where

import Prelude hiding (readFile, lines)

import Control.Monad ((<$!>), when, mapM, forM_, foldM_)
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
    -- cache directory (also creates the build dir)
    createDirectoryIfMissing True $!
        cmdBuildDir cmd ++ "/Cache"

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
    let verb = cmdVerb cmd
    let buildDir = cmdBuildDir cmd ++ "/" ++
            modPathToRelDir relPath

    when (cmdTrace cmd) $!
        createDirectoryIfMissing True buildDir
    message verb "Building Module [%s]\n" [modName]
    debug verb "module build dir: %s\n" [buildDir]

    src <- makeAbsolute relPath >>= readFile

    parseRes <- parseFile cmd src modName
    trace cmd (buildDir ++ "Abstract-Syntax-Tree.txt")
        (concat $ pretty <$!> parseRes)

    analysisRes <- analyzeFile cmd src modName parseRes
    trace cmd (buildDir ++ "Symbol-Table.txt")
        (detailed $! arTable analysisRes)

    -- status verb "Finished Building [%s]\n" [modName]


parseFile :: CmdLine -> Text -> ModuleName -> IO [Expr]
parseFile cmd src name = do
    info (cmdVerb cmd) "Parsing   [%s]\n" [name]
    case parse roseParser name src of
        Left err -> do
            printParseErr (cmdVerb cmd) err src
            fatal (cmdVerb cmd)
                "Failed while parsing module (%s)\n"
                [name]
        Right exprs -> return exprs


analyzeFile :: CmdLine -> Text -> ModuleName -> [Expr]
            -> IO (Analysis ())
analyzeFile cmd src name es = do
    let res = analyze cmd $!
            foldM_ (\_ -> infer_) () es
    info (cmdVerb cmd) "Analyzing [%s]\n" [name]
    if null $! arErrors res then
        return res
    else let lns = T.lines src in do
        mapM (putStrLn . prettyError cmd lns) (arErrors res)
        putStrLn ""
        return res
        fatal (cmdVerb cmd)
            "Failed while analyzing module (%s)\n"
            [name]
