module Parser (
    module Parser.Components.Imports,
    module Parser.Components.Pragmas,
    ParseResults(..),
    parser,
) where

import Data.Either (partitionEithers)
import Text.Parsec

import Parser.Components.Datatypes
import Parser.Components.Functions
import Parser.Components.Identifiers
import Parser.Components.Imports
import Parser.Components.Internal.LangDef
import Parser.Components.Pragmas
import Parser.Components.Traits
import Parser.Data


default (Int, Double)


data ParseResults = ParseResults {
        prImports :: [Import],
        prParseTree :: [Expr],
        prPragmas :: [Pragma]
    }            


-- (i guess we're ignoring pragmas for now)
-- = module-header, {import}, {top-level-decl}
parser :: Parser ParseResults
parser = do
    _ <- header
    -- expName <- sourceName <$> getPosition
    imps <- many moduleImport
    topLevel <- manyTill topLevelDecl eof
    let (pragmas, tree) =
            partitionEithers topLevel
    return (ParseResults imps tree pragmas)

-- = "module", big-ident, "where";
header :: Parser String
header = (do
    keyword "module"
    name <- moduleName
    keyword "where"
    return name
    ) <?> "module header"

-- = pragma-seq
-- | func-decl | func-def
-- | data-def | type-alias
-- | trait-decl | trait-impl;
topLevelDecl :: Parser (Either Pragma Expr)
topLevelDecl = choice [
    Left <$> pragma,
    Right <$> traitDecl,
    Right <$> traitImpl,
    Right <$> funcDecl,
    Right <$> funcDef,
    Right <$> dataDef,
    Right <$> typeAlias
    ] <?> "top-level declaration"

-- roseParser :: SourcePos -> Parser [Expr]
-- roseParser _ = return []
-- roseParser pos = setPosition pos >> manyTill (choice [
--         Pragma <$> pragma,
--         funcDef,
--         try funcTypeDecl,
--         try dataDef,
--         try traitDecl,
--         traitImpl
--     ]) eof

-- importsParser ::
--     Parser (String, [Import], Text, SourcePos)
-- importsParser = return ([], [], empty, initialPos "")
-- importsParser = do
--     wspace
--     modName <- moduleDecl
--     imports <- many modImport
--     rest <- getInput
--     pos <- getPosition
--     return (varName modName, imports, rest, pos)

-- moduleDecl :: Parser Variable
-- moduleDecl = (do
--     keyword "module"
--     name <- moduleName
--     keyword "where"
--     return $ name
--     ) <?> "module declaration"

-- modImport :: Parser ImportModule
-- modImport = (do
--     keyword "import"
--     vis <- option Intern visibility'
--     name <- moduleName
--     alias <- option name $ do
--         keyword "as"
--         moduleName <?> "module alias"
--     imps <- optionMaybe $ do
--         keyword "using"
--         braces (commaSepEnd idenImport)
--             <?> "import list"
--     return $ Import (varName name) (varName alias) vis imps
--     ) <?> "module import"

-- idenImport :: Parser ImportIden
-- {-# INLINABLE idenImport #-}
-- idenImport = try traitImp <|> try dataImp <|> foImp
--     where
--         {-# INLINE traitImp #-}
--         traitImp = (do
--             keyword "trait"
--             ImportedTrait . varName <$!> bigIden
--             ) <?> "trait name"
--         {-# INLINE dataImp #-}
--         dataImp = (do
--             keyword "data"
--             ImportedType . varName <$!> bigIden
--             ) <?> "datatype name"
--         {-# INLINE foImp #-}
--         foImp = ImportedFunc . varName <$!> foName
