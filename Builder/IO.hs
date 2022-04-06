module Builder.IO where

import Builder.Builder


readFilePath :: FilePath -> BuilderIO Stream
readFilePath path = do
    setFilePath path
    src <- readFile <#> path
    setSource src
    return src


readFilePath_ :: FilePath -> BuilderIO ()
readFilePath_ path = do
    setFilePath path
    src <- readFile <#> path
    setSource src
