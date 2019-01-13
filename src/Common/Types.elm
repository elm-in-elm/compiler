module Common.Types exposing
    ( Dict_
    , ElmProgram(..)
    , FileContents(..)
    , FilePath(..)
    , Module
    , ModuleName(..)
    , Project
    , ProjectToEmit
    , Set_
    )

{-| TODO is this an OK way to do the module hierarchy? Is there a better one?
-}

import AST.Backend as Backend
import AST.Canonical as Canonical
import AST.Common exposing (TopLevelDeclaration, VarName)
import AST.Frontend as Frontend
import Dict.Any as AnyDict exposing (AnyDict)
import Elm.Project
import Set.Any as AnySet exposing (AnySet)


type alias Set_ a =
    AnySet String a


type alias Dict_ a b =
    AnyDict String a b


type FilePath
    = FilePath String


type ModuleName
    = ModuleName String


type FileContents
    = FileContents String


type alias Project =
    { mainFilePath : FilePath
    , mainModuleName : ModuleName
    , elmJson : Elm.Project.Project
    , {- TODO allow multiple source directories -} sourceDirectory : FilePath
    , program : ElmProgram
    }


type alias ProjectToEmit =
    { output : FileContents
    }


type alias Modules expr =
    Dict_ ModuleName (Module expr)


type ElmProgram
    = Frontend { modules : Modules Frontend.Expr }
    | Canonical { modules : Modules Canonical.Expr }
    | Backend { modules : Modules Backend.Expr }


type alias Module expr =
    { dependencies : Set_ ModuleName -- ie. imports. TODO will have to contain the `as` and `exposing` information later
    , name : ModuleName
    , filePath : FilePath
    , topLevelDeclarations : Dict_ VarName (TopLevelDeclaration expr)
    }
