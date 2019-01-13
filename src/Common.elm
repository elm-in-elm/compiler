module Common exposing
    ( Dict_
    , ElmProgram(..)
    , FileContents(..)
    , FilePath(..)
    , Module
    , ModuleName(..)
    , Project
    , ProjectToEmit
    , Set_
    , expectedFilePath
    , expectedModuleName
    , filePathToString
    , moduleNameToString
    , moduleNames
    )

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


filePathToString : FilePath -> String
filePathToString (FilePath filePath) =
    filePath


type ModuleName
    = ModuleName String


moduleNameToString : ModuleName -> String
moduleNameToString (ModuleName moduleName) =
    moduleName


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


moduleNames : ElmProgram -> Set_ ModuleName
moduleNames program =
    let
        toSet : Dict_ ModuleName a -> Set_ ModuleName
        toSet dict =
            dict
                |> AnyDict.keys
                |> AnySet.fromList moduleNameToString
    in
    case program of
        Frontend { modules } ->
            toSet modules

        Canonical { modules } ->
            toSet modules

        Backend { modules } ->
            toSet modules


type alias Module expr =
    { dependencies : Set_ ModuleName -- ie. imports. TODO will have to contain the `as` and `exposing` information later
    , name : ModuleName
    , filePath : FilePath
    , topLevelDeclarations : Dict_ VarName (TopLevelDeclaration expr)
    }


expectedFilePath : FilePath -> ModuleName -> FilePath
expectedFilePath (FilePath sourceDirectory) (ModuleName moduleName) =
    {- TODO somewhere normalize the / out of the source directories
       so that it's not there twice.

       Eg. we wouldn't want

           sourceDirectories = ["src/"]
           --> expectedFilePaths ... == "src//Foo.elm"
    -}
    FilePath (sourceDirectory ++ "/" ++ String.replace "." "/" moduleName ++ ".elm")


expectedModuleName : FilePath -> FilePath -> Maybe ModuleName
expectedModuleName (FilePath sourceDirectory) (FilePath filePath) =
    if String.startsWith sourceDirectory filePath then
        let
            lengthToDrop : Int
            lengthToDrop =
                String.length sourceDirectory
        in
        filePath
            |> String.dropLeft lengthToDrop
            |> String.replace "/" "."
            -- remove the ".elm":
            |> String.dropRight 4
            |> ModuleName
            |> Just

    else
        Nothing
