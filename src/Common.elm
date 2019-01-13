module Common exposing
    ( Dict_
    , FileContents(..)
    , FilePath(..)
    , Module
    , ModuleName(..)
    , Project
    , Set_
    , TopLevelDeclaration
    , VarName
    , expectedFilePath
    , expectedModuleName
    , filePathToString
    , moduleNameToString
    )

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
    , modules : Dict_ ModuleName Module
    }


type alias Module =
    { dependencies : Set_ ModuleName -- ie. imports. TODO will have to contain the `as` and `exposing` information later
    , name : ModuleName
    , filePath : FilePath
    , topLevelDeclarations : Dict_ VarName TopLevelDeclaration
    }


type VarName
    = VarName String


type Expr
    = Var VarName
    | Application
        { fn : Expr
        , arg : Expr
        }
    | Lambda
        { argName : VarName
        , body : Expr
        }
    | Let
        { varName : VarName
        , varBody : Expr
        , body : Expr
        }
    | Literal Literal
    | If
        { test : Expr
        , then_ : Expr
        , else_ : Expr
        }
    | Fixpoint Expr
    | Operator
        { opName : VarName
        , left : Expr
        , right : Expr
        }


type Literal
    = LInt Int
      -- TODO | LFloat Float
    | LBool Bool -- TODO how to do this and have Bools defined in the elm/core instead of hardcoded in the compiler?


type alias TopLevelDeclaration =
    { name : VarName
    , body : Expr
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
