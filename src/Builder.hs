module Builder (
    module Builder,
) where

import Control.Monad (forM_)

import Builder.Internal as Builder
import Builder.IO as Builder


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
        _ <- analyzeExprs tree
        finalizeVisit
        forM_ imports $ \(Import (Var modName _) _) ->
            buildFile (modToPath modName)
