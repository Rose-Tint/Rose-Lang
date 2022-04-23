module Parser.Parser (roseParser, importsParser) where

import Text.Parsec
import Text.Parsec.Pos
import Data.Text (Text, empty)

import Parser.Components.Imports
import Parser.Data


default (Int, Double)


roseParser :: SourcePos -> Parser [Expr]
roseParser _ = return []
-- roseParser pos = setPosition pos >> manyTill (choice [
--         Pragma <$> pragma,
--         funcDef,
--         try funcTypeDecl,
--         try dataDef,
--         try traitDecl,
--         traitImpl
--     ]) eof

importsParser ::
    Parser (String, [Import], Text, SourcePos)
importsParser = return ([], [], empty, initialPos "")
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
