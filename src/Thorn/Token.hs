module Thorn.Token (
    Token(..),
) where

import Common.Module
import Text.Pretty


data Token
    = TName String
    | TPath FilePath
    | TModule ModName
    | TComma
    | TSemi
    | TColon
    | TRBrace
    | TLBrace
    | KW_project
    | KW_build_dir
    | KW_source_dir
    | KW_modules
    | KW_executable
    | KW_library
    | TEOF
    deriving (Eq)


instance Pretty Token where
    pretty (TName name) = pretty name
    pretty (TPath path) = pretty path
    pretty (TModule name) = pretty name
    pretty TComma = ","
    pretty TColon = ":"
    pretty TSemi = ";"
    pretty TRBrace = "}"
    pretty TLBrace = "{"
    pretty KW_project = "project"
    pretty KW_build_dir = "build-dir"
    pretty KW_source_dir = "source-dir"
    pretty KW_modules = "modules"
    pretty KW_executable = "executable"
    pretty KW_library = "library"
    pretty TEOF = "{- END OF INPUT -}"
    detailed (TName name) = "`"+|name|+"` (name)"
    detailed (TPath path) = "`"+|path|+"` (path)"
    detailed (TModule name) = "`"+|name|+"` (module)"
    detailed TComma = "','"
    detailed TColon = "':'"
    detailed TSemi = "';'"
    detailed TRBrace = "'}'"
    detailed TLBrace = "'{'"
    detailed KW_project = "'project' (keyword)"
    detailed KW_build_dir = "'build-dir' (keyword)"
    detailed KW_source_dir = "'source-dir' (keyword)"
    detailed KW_modules = "'modules' (keyword)"
    detailed KW_executable = "'executable' (keyword)"
    detailed KW_library = "'library' (keyword)"
    detailed TEOF = "{- END OF INPUT -}"
