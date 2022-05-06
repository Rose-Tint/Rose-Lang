module Builder (
    build,
    buildM,
) where

import Control.Monad (forM_)

import Common.Var
import Middle.Analyzer
import Builder.CmdLine
import Builder.Internal
import Builder.IO
import Builder.Output
import Front.Parser
import Pretty
import Utils.Paths (modToPath)


default (Int, Double)


build :: BuilderIO ()
build = getSourceFiles >>= mapM_ buildFile

buildFile :: FilePath -> BuilderIO ()
buildFile [] = return ()
buildFile path = hasBeenVisited path >>= \skip ->
    if skip then return () else do
        mReadFile path
        name <- stModule <$> getState
        message ("Building Module ["+|name|+"]\n")
        Module imports tree <- parseFile
        _ <- analyzeFile tree
        finalizeVisit
        forM_ imports $ \(Import (Var modName _) _) ->
            buildFile (modToPath modName)

parseFile :: BuilderIO Module
parseFile = do
    name <- getModule
    debug ("Parsing   ["+|name|+"]\n")
    src <- getSource
    case parse src of
        Left msg -> fatal $ Red|+|name|+|msg|+"\n"
        Right parseTree -> do
            trace "Parse-Tree.txt" parseTree
            return parseTree

analyzeFile :: [Expr] -> BuilderIO Analysis
analyzeFile es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    let !res = analyze es
    trace "Symbol-Table.txt" (arTable res)
    printAnalysisErrors $ arErrors res
    return res

printAnalysisErrors :: [ErrInfo] -> BuilderIO ()
printAnalysisErrors es = do
    lns <- lines <$> getSource
    name <- stModule <$> getState
    forM_ es $ \e ->
        message $ name|+|(lns, e)
