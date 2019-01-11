module Main exposing (main)

import Common
    exposing
        ( AST
        , Dict_
        , FileContents(..)
        , FilePath(..)
        , Module
        , ModuleName(..)
        , Project
        , Set_
        )
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Dict.Extra as Dict
import Elm.Project
import Error exposing (Error(..), ParseError(..))
import Json.Decode as JD
import Platform
import Ports
    exposing
        ( println
        , printlnStderr
        , readFile
        , waitForReadFile
        , writeToFile
        )
import Set exposing (Set)
import Set.Any as AnySet exposing (AnySet)
import Stage.Emit as Emit
import Stage.Optimize as Optimize
import Stage.Parse as Parse
import Stage.Typecheck as Typecheck


{-| We're essentially a Node.JS app (until we get self-hosting :P ).
So, `Platform.worker` is the only option for us.
-}
main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { -- TODO allow for multiple `main`s instead of just one
      mainFilePath : String
    , elmJson : String
    }


type Model
    = Compiling Model_
    | EncounteredError
    | Finished


type alias Model_ =
    { project : Project
    , waitingForFiles : Set_ FilePath
    }


type Msg
    = -- TODO ReadFileFailure
      ReadFileSuccess FilePath FileContents


init : Flags -> ( Model, Cmd Msg )
init ({ mainFilePath, elmJson } as flags) =
    let
        mainFilePath_ : FilePath
        mainFilePath_ =
            FilePath mainFilePath

        elmJson_ : Result Error Elm.Project.Project
        elmJson_ =
            JD.decodeString Elm.Project.decoder elmJson
                |> Result.mapError (ParseError << InvalidElmJson)

        sourceDirectory : Result Error FilePath
        sourceDirectory =
            elmJson_
                |> Result.andThen getSourceDirectory

        mainModuleName_ : Result Error ModuleName
        mainModuleName_ =
            sourceDirectory
                |> Result.andThen
                    (\sourceDirectory_ ->
                        expectedModuleName sourceDirectory_ mainFilePath_
                            {- TODO It is probably not enforced by the official compiler
                               for the main module's path to be included in the source directories.
                               We'd have to read the module name from the file contents in that case.
                               Check that assumption and do that!
                            -}
                            |> Result.fromMaybe (ParseError (MainModuleNotInSourceDirectory mainFilePath_))
                    )

        modelAndCmd : Result Error ( Model, Cmd Msg )
        modelAndCmd =
            Result.map3
                (\mainModuleName elmJson__ sourceDirectory_ ->
                    ( Compiling
                        { project =
                            { mainFilePath = mainFilePath_
                            , mainModuleName = mainModuleName
                            , elmJson = elmJson__
                            , sourceDirectory = sourceDirectory_
                            , modules = AnyDict.empty Common.moduleNameToString
                            }
                        , waitingForFiles = AnySet.singleton mainFilePath_ Common.filePathToString
                        }
                    , readFile mainFilePath_
                    )
                )
                mainModuleName_
                elmJson_
                sourceDirectory
    in
    case modelAndCmd of
        Ok modelAndCmd_ ->
            modelAndCmd_

        Err err ->
            ( EncounteredError
            , printlnStderr (Error.toString err)
            )


getSourceDirectory : Elm.Project.Project -> Result Error FilePath
getSourceDirectory elmProject =
    case elmProject of
        Elm.Project.Application { dirs } ->
            dirs
                |> List.head
                |> Maybe.map FilePath
                |> Result.fromMaybe (ParseError EmptySourceDirectories)

        Elm.Project.Package _ ->
            Ok (FilePath "src/")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        EncounteredError ->
            ( model, Cmd.none )

        Compiling model_ ->
            update_ msg model_

        Finished ->
            ( model, Cmd.none )


update_ : Msg -> Model_ -> ( Model, Cmd Msg )
update_ msg model =
    case log msg of
        ReadFileSuccess filePath fileContents ->
            handleReadFileSuccess filePath fileContents model


handleReadFileSuccess : FilePath -> FileContents -> Model_ -> ( Model, Cmd Msg )
handleReadFileSuccess filePath fileContents ({ project } as model) =
    case Parse.parse filePath fileContents of
        Err error ->
            handleError error

        Ok ({ name, dependencies } as parsedModule) ->
            let
                modulesToBeRead : Set_ ModuleName
                modulesToBeRead =
                    dependencies
                        |> AnySet.diff
                            (project.modules
                                |> AnyDict.keys
                                |> AnySet.fromList Common.moduleNameToString
                            )

                filesToBeRead : Set_ FilePath
                filesToBeRead =
                    modulesToBeRead
                        |> AnySet.map
                            Common.filePathToString
                            (expectedFilePath project.sourceDirectory)

                newModules : Dict_ ModuleName Module
                newModules =
                    project.modules
                        |> AnyDict.update name
                            (Maybe.map (always parsedModule))

                newProject : Project
                newProject =
                    { project | modules = newModules }

                newWaitingForFiles : Set_ FilePath
                newWaitingForFiles =
                    model.waitingForFiles
                        |> AnySet.union filesToBeRead

                newModel : Model_
                newModel =
                    { model
                        | project = newProject
                        , waitingForFiles = newWaitingForFiles
                    }
            in
            if AnySet.isEmpty newWaitingForFiles then
                compile newProject

            else
                ( Compiling newModel
                , filesToBeRead
                    |> AnySet.toList
                    |> List.map readFile
                    |> Cmd.batch
                )


{-| We're done reading and parsing files. Now we can do the rest synchronously!
-}
compile : Project -> ( Model, Cmd Msg )
compile project =
    Ok project
        |> Result.andThen Typecheck.typecheck
        |> Result.andThen Optimize.optimize
        |> Result.andThen Emit.emit
        |> finish


finish : Result Error FileContents -> ( Model, Cmd Msg )
finish result =
    case result of
        Ok output ->
            ( Finished
            , Cmd.batch
                [ writeToFile (FilePath "out.js") output
                , println "Compilation finished, wrote output to `out.js`."
                ]
            )

        Err error ->
            handleError error


{-| When an error happens, we bail out as early as possible.
Stop everything, abort mission, jump ship!

The `EncounteredError` model stops most everything (`update`, `subscriptions`).

-}
handleError : Error -> ( Model, Cmd Msg )
handleError error =
    ( EncounteredError
    , printlnStderr (Error.toString error)
    )


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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        EncounteredError ->
            Sub.none

        Compiling model_ ->
            waitForReadFile ReadFileSuccess

        Finished ->
            Sub.none


log : Msg -> Msg
log msg =
    let
        string =
            case msg of
                ReadFileSuccess _ _ ->
                    "ReadFileSuccess"

        _ =
            Debug.log string ()
    in
    msg
