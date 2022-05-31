{
module Thorn.Parser (
    runAlex,
    thornP,
) where

import Common.Module
import Text.Pretty
import Thorn.Lexer
import Thorn.Project
import Thorn.Token

}

%name thornP Project
%error { parseError }
%lexer { lexer } { TEOF }
%monad { Alex }

%tokentype { Token }
%token
    name            { TName $$ }
    path            { TPath $$ }
    module          { TModule $$ }
    -- keywords
    build_dir       { KW_build_dir }
    source_dir      { KW_source_dir }
    project         { KW_project }
    modules         { KW_modules }
    executable      { KW_executable }
    library         { KW_library }
    -- reserved symbols
    ":"             { TColon }
    ";"             { TSemi }
    ","             { TComma }
    -- groupers
    "{"             { TLBrace }
    "}"             { TRBrace }


%%


Project :: { Project } :
    ProjectDecl
    {- GlobalOpts -}
    Components
    { Proj $1 $2 }

ProjectDecl :: { String }
    : project ":" name ";"    { $3 }

Components :: { [Component] }
    : Component Components    { ($1:$2) }
    | Component    { [$1] }

Component :: { Component }
    : executable CompName ":" "{"
        BuildDir
        SourceDir
        Modules "}"
        { Exec $2 $5 $6 $7 }
    | library CompName ":" "{"
        BuildDir
        SourceDir
        Modules "}"
        { Lib $2 $5 $6 $7 }

CompName :: { Maybe String }
    : name    { Just $1 }
    | {- empty -}    { Nothing }

BuildDir :: { Maybe FilePath }
    : build_dir ":" path ","    { Just $3 }
    | {- empty -}    { Nothing }

SourceDir :: { Maybe FilePath }
    : source_dir ":" path ","    { Just $3 }
    | {- empty -}    { Nothing }

Modules :: { [ModName] }
    : modules ":" "{" ModuleList "}"    { $4 }

ModuleList :: { [ModName] }
    : module "," ModuleList    { ($1:$3) }
    | module ","    { [$1] }
    | module    { [$1] }

{
parseError :: Token -> Alex a
parseError = lexError . detailed
}
