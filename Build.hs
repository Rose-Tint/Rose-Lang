module Build where

import Prelude hiding (readFile, lines)

-- import Control.Concurrent (forkIO)
import Control.Monad (when, foldM_)
import Data.Text (Text)
import Data.Text.IO (readFile)
import System.Directory
import System.IO ()
import Text.Parsec (parse)

import Cache
import CmdLine (CmdLine(..))
import Parser.Data (Expr)
import Parser.Error (printParseErr)
import Parser.Parser (roseParser)
import Parser.Pretty (prettyExpr)
import SymbolTable
import Output
import Utils (pathToModule, modPathToRelDir)


type ModuleName = String


type BuildResult = ()



build :: CmdLine -> IO BuildResult
build cmd = do
    -- cache directory (also creates the build dir)
    createDirectoryIfMissing True $!
        cmdBuildDir cmd ++ "/Cache"
    -- foldM_ (\_ file -> (forkIO $! buildFile cmd file)
    --         >> return ())
    --     () (cmdFiles cmd)
    foldM_ (\_ -> buildFile cmd)
        () (cmdFiles cmd)
    return ()


buildFile :: CmdLine -> FilePath -> IO BuildResult
buildFile cmd relPath = do
    let modName = pathToModule relPath
    let verb = cmdVerb cmd
    let buildDir = cmdBuildDir cmd ++ "/" ++
            modPathToRelDir relPath
    isUTD <- isUpToDate cmd relPath
    when isUTD $!
        message verb "Up To Date        [%s]" [modName]
    cacheBuildTime cmd relPath
    when (cmdTrace cmd) $!
        createDirectoryIfMissing True buildDir
    message verb "Building Module   [%s]\n" [modName]
    debug verb "module build dir: %s\n" [buildDir]
    src <- makeAbsolute relPath >>= readFile
    parseRes <- parseFile cmd src modName
    trace cmd (buildDir ++ "Abstract-Syntax-Tree.txt")
        (concat $ fmap prettyExpr parseRes)
    symTbl <- analyzeFile cmd src modName parseRes
    trace cmd (buildDir ++ "Symbol-Table.txt")
        (show symTbl)
    status verb "Finished Building [%s]\n" [modName]
    return ()


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
analyzeFile _ _ _ _ = return emptyTable
