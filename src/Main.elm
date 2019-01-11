module Main exposing (main)

import Common
    exposing
        ( AST
        , Dict_
        , FileContents(..)
        , FilePath(..)
        , Module
        , ModuleName(..)
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
    { mainFilePath : FilePath
    , mainModuleName : ModuleName
    , -- TODO allow for multiple source directories
      sourceDirectory : FilePath
    , modules : Dict_ ModuleName Module
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

        sourceDirectory : Result Error FilePath
        sourceDirectory =
            JD.decodeString Elm.Project.decoder elmJson
                |> Result.mapError (ParseError << InvalidElmJson)
                |> Result.andThen
                    (\elmProject ->
                        case elmProject of
                            Elm.Project.Application { dirs } ->
                                dirs
                                    |> List.head
                                    |> Maybe.map FilePath
                                    |> Result.fromMaybe (ParseError EmptySourceDirectories)

                            Elm.Project.Package _ ->
                                Ok (FilePath "src/")
                    )

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
            Result.map2
                (\mainModuleName sourceDirectory_ ->
                    ( Compiling
                        { mainFilePath = mainFilePath_
                        , mainModuleName = mainModuleName
                        , sourceDirectory = sourceDirectory_
                        , modules = AnyDict.empty Common.moduleNameToString
                        , waitingForFiles = AnySet.singleton mainFilePath_ Common.filePathToString
                        }
                    , readFile mainFilePath_
                    )
                )
                mainModuleName_
                sourceDirectory
    in
    case modelAndCmd of
        Ok modelAndCmd_ ->
            modelAndCmd_

        Err err ->
            ( EncounteredError
            , printlnStderr (Error.toString err)
            )



{-
   mainFilename
     -- FRONTEND
       |> collectAndParseSources
       |> typecheck
     -- OPTIMIZERS
       |> eliminateDeadCode
       |> evaluate
     -- BACKEND
       |> emitJS
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        EncounteredError ->
            ( model, Cmd.none )

        Compiling model_ ->
            update_ msg model_


update_ : Msg -> Model_ -> ( Model, Cmd Msg )
update_ msg model =
    case log msg of
        ReadFileSuccess filePath fileContents ->
            handleReadFileSuccess filePath fileContents model


handleReadFileSuccess : FilePath -> FileContents -> Model_ -> ( Model, Cmd Msg )
handleReadFileSuccess filePath fileContents model =
    case Parse.parse filePath fileContents of
        Err error ->
            handleError error

        Ok ({ name, dependencies } as parsedModule) ->
            let
                modulesToBeRead : Set_ ModuleName
                modulesToBeRead =
                    dependencies
                        |> AnySet.diff
                            (model.modules
                                |> AnyDict.keys
                                |> AnySet.fromList Common.moduleNameToString
                            )

                filesToBeRead : Set_ FilePath
                filesToBeRead =
                    modulesToBeRead
                        |> AnySet.map Common.filePathToString (expectedFilePath model.sourceDirectory)

                newModules : Dict_ ModuleName Module
                newModules =
                    model.modules
                        |> AnyDict.update name
                            (Maybe.map (always parsedModule))

                newWaitingForFiles : Set_ FilePath
                newWaitingForFiles =
                    model.waitingForFiles
                        |> AnySet.union filesToBeRead

                newModel : Model
                newModel =
                    Compiling
                        { model
                            | modules = newModules
                            , waitingForFiles = newWaitingForFiles
                        }
            in
            if AnySet.isEmpty newWaitingForFiles then
                -- We're done reading and parsing files. Now we can do the rest synchronously!
                Ok newModules
                    |> Result.andThen Typecheck.typecheck
                    |> Result.andThen Optimize.optimize
                    |> Result.andThen Emit.emit
                    |> finish

            else
                ( newModel
                , filesToBeRead
                    |> AnySet.toList
                    |> List.map readFile
                    |> Cmd.batch
                )


finish : Result Error FileContents -> ( Model, Cmd Msg )
finish result =
    case result of
        Ok output ->
            ( Finished
            , writeToFile (FilePath "out.js") output
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
