module Build where

import Prelude hiding (readFile, lines)

import Control.Monad ((<$!>), when, foldM_)
import Data.Text (Text)
import Data.Text.IO (readFile)
import System.Directory
import System.IO ()
import Text.Parsec (parse)

import CmdLine (CmdLine(..))
import Analyzer.Analyzer
import Parser.Data (Expr)
import Parser.Error (printParseErr)
import Parser.Parser (roseParser)
import Typing.Checker
import Output
import Pretty
import Threading
import Utils


type ModuleName = String


type BuildResult = ()



build :: CmdLine -> IO BuildResult
build cmd = do
    -- cache directory (also creates the build dir)
    createDirectoryIfMissing True $!
        cmdBuildDir cmd ++ "/Cache"
    mgr <- newManager
    foldM_ (\_ file -> do
            fork mgr $! buildFile cmd file
            return ()
        ) () (cmdFiles cmd)
    waitAll mgr
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

    _ <- analyzeFile cmd src modName parseRes

    symTbl <- analyzeFile cmd src modName parseRes
    trace cmd (buildDir ++ "Symbol-Table.txt")
        (show symTbl)

    -- status verb "Finished Building [%s]\n" [modName]


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
            -> IO (Analysis ())
analyzeFile cmd _ _ es = return $!
    analyze cmd $! foreachM_ es infer
