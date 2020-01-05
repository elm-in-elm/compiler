module Main exposing (main)

{-| Welcome to the Elm compiler written in Elm!

To get things out of the way: it would be great if the pipeline could be pure:

    input
        |> parse
        |> desugar
        |> inferTypes
        |> optimize
        |> emit

But we have to read files and parse them (to, in turn, find more files to read!),
so the code doesn't look as nice and we have to thread the reading and parsing
through The Elm Architecture.

The reading is done with ports (as Elm doesn't have an general-purpose
filesystem IO library) and a little bit of JavaScript: see `src/index.js`.

    ┌─────────────────────────────────┐      ┌─────┐
    │ Main file(s) (cmdline argument) │      │ Die │
    └───────────────┬─────────────────┘      └─────┘
                    │                           ^
                    │                           │ if syntax error
                    v                           │
         ┌──────────────────────┐      ┌────────┴───────┐      ┌───────────────────┐ if all read  ┌─────────────────────────────┐
         │ Read from filesystem ├─────>│ Parse into AST ├─────>│ Find dependencies ├─────────────>│ Rest of the compile process │
         └──────────────────────┘      └────────────────┘      └─────────┬─────────┘              └─────────────────────────────┘
                    ^                                                    │
                    │                                                    │ if new file to read
                    └────────────────────────────────────────────────────┘

The loop at the bottom left is the bulk of this file.

The rest of the compile process can be found in the function `compile`.
You'll find it has to deal with `Result`s, because the compilation stages
can return errors. Of course, Elm doesn't have exceptions, so we are explicit
about returning those.

-}

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error as Error exposing (Error(..), ParseError(..))
import Elm.Data.Declaration as Declaration
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath as FilePath exposing (FilePath)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName as ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Project
import Json.Decode as JD
import Platform
import Ports exposing (println, printlnStderr)
import Set exposing (Set)
import Stage.Desugar as Desugar
import Stage.Emit.JavaScript as EmitJS
import Stage.Emit.JsonAST as EmitJson
import Stage.InferTypes as InferTypes
import Stage.Optimize as Optimize
import Stage.Parse as Parse


{-| We're essentially a Node.JS app (until we get self-hosting :P ).
So, `Platform.worker` is the only option for us.
-}
main : Program Flags (Model Frontend.ProjectFields) Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { mainFilePath : String
    , elmJson : String
    , outputFormat : String
    }


{-| `Compiling` is the state we'll be most of the time. The other two are
mostly useless; they do effectively stop `subscriptions` and `update` though.
-}
type Model projectFields
    = Compiling (Model_ projectFields)
    | {- We don't need to remember the error, because we report it
         at the time of transition to this new model. See `handleError`.
      -}
      EncounteredError
    | Finished


{-| Because we're mostly in the `Compiling` state, it is worthwhile
to make functions work with its data instead of the general `Model`.
-}
type alias Model_ projectFields =
    { project : Project projectFields
    , waitingForFiles : Set FilePath
    , outputFormat : String
    }


type Msg
    = ReadFileSuccess { filePath : FilePath, fileContents : FileContents }
    | ReadFileError ErrorCode -- already contains the FilePath


subscriptions : Model expr -> Sub Msg
subscriptions model =
    case model of
        Compiling _ ->
            {- We'll be waiting for the various file contents we've asked for
               with `readFile`, but only on the happy path. They are of no use
               to us when we've already found an error elsewhere or finished
               the compilation.
            -}
            Ports.waitForReadFile
                (ReadFileError << parseErrorCode)
                ReadFileSuccess

        EncounteredError ->
            Sub.none

        Finished ->
            Sub.none


{-| The JS wrapper gives us the main filepath and the contents of elm.json.
We have two tasks here:

  - decode the elm.json file to something meaningful
  - read the main module (TODO maybe do that even before `init` in JS?)

-}
init : Flags -> ( Model Frontend.ProjectFields, Cmd Msg )
init { mainFilePath, elmJson, outputFormat } =
    let
        elmJsonProject : Result Error Elm.Project.Project
        elmJsonProject =
            JD.decodeString (JD.map normalizeDirs Elm.Project.decoder) elmJson
                |> Result.mapError (ParseError << InvalidElmJson)

        sourceDirectory : Result CLIError FilePath
        sourceDirectory =
            elmJsonProject
                |> Result.andThen getSourceDirectory
                |> Result.mapError CompilerError

        mainModuleName : Result CLIError ModuleName
        mainModuleName =
            sourceDirectory
                |> Result.andThen
                    (\sourceDirectory_ ->
                        {- TODO It is probably not enforced by the official compiler
                           for the main module's path to be included in the source directories.
                           We'd have to read the module name from the file contents in that case.
                           Check that assumption and do the right thing!
                        -}
                        ModuleName.expectedModuleName
                            { sourceDirectory = sourceDirectory_
                            , filePath = mainFilePath
                            }
                            -- TODO this conversion to Result is duplicated. We should really return the Result Error ... in the Name.expectedModuleName!
                            |> Result.fromMaybe (FileNotInSourceDirectories mainFilePath)
                    )

        modelAndCmd : Result CLIError ( Model Frontend.ProjectFields, Cmd Msg )
        modelAndCmd =
            Result.map3
                (\mainModuleName_ elmJsonProject_ sourceDirectory_ ->
                    ( Compiling
                        { project =
                            { mainFilePath = mainFilePath
                            , mainModuleName = mainModuleName_
                            , elmJson = elmJsonProject_
                            , sourceDirectory = sourceDirectory_
                            , modules = Dict.empty
                            }
                        , waitingForFiles = Set.singleton mainFilePath
                        , outputFormat = outputFormat
                        }
                    , Ports.readFile mainFilePath
                    )
                )
                mainModuleName
                (elmJsonProject |> Result.mapError CompilerError)
                sourceDirectory
    in
    case modelAndCmd of
        Ok modelAndCmd_ ->
            modelAndCmd_

        Err error ->
            handleError error


normalizeDirs : Elm.Project.Project -> Elm.Project.Project
normalizeDirs project =
    case project of
        Elm.Project.Application data ->
            Elm.Project.Application
                { data
                    | dirs =
                        List.map
                            FilePath.removeTrailingSlash
                            data.dirs
                }

        Elm.Project.Package data ->
            Elm.Project.Package data


{-| Applications tell us their source directories; packages have `src/`.
-}
getSourceDirectory : Elm.Project.Project -> Result Error FilePath
getSourceDirectory elmProject =
    case elmProject of
        Elm.Project.Application { dirs } ->
            dirs
                |> {- TODO allow multiple source directories -} List.head
                |> Result.fromMaybe (ParseError EmptySourceDirectories)

        Elm.Project.Package _ ->
            -- TODO is it OK that this has the trailing slash?
            Ok "src/"


update : Msg -> Model Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
update msg model =
    case model of
        Compiling model_ ->
            update_ msg model_

        EncounteredError ->
            ( model, Cmd.none )

        Finished ->
            ( model, Cmd.none )


update_ : Msg -> Model_ Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
update_ msg model =
    case {- log -} msg of
        ReadFileSuccess file ->
            handleReadFileSuccess file model

        ReadFileError errorCode ->
            handleReadFileError errorCode


handleReadFileSuccess : { filePath : FilePath, fileContents : FileContents } -> Model_ Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileSuccess ({ filePath } as file) ({ project } as model) =
    let
        parseResult =
            Parse.parse file
                |> Result.andThen
                    (checkModuleNameAndFilePath
                        { sourceDirectory = project.sourceDirectory
                        , filePath = filePath
                        }
                    )
    in
    case parseResult of
        Err error ->
            handleError <| CompilerError error

        Ok ({ name, imports } as parsedModule) ->
            let
                filesToBeRead : Set FilePath
                filesToBeRead =
                    imports
                        |> Dict.keys
                        |> List.map
                            (\moduleName ->
                                FilePath.expectedFilePath
                                    { sourceDirectory = project.sourceDirectory
                                    , moduleName = moduleName
                                    }
                            )
                        |> Set.fromList

                newModules : Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation)
                newModules =
                    Dict.update name
                        (always (Just parsedModule))
                        project.modules

                newProject : Project Frontend.ProjectFields
                newProject =
                    { project | modules = newModules }

                newWaitingForFiles : Set FilePath
                newWaitingForFiles =
                    model.waitingForFiles
                        |> Set.union filesToBeRead
                        |> Set.remove filePath

                newModel : Model_ Frontend.ProjectFields
                newModel =
                    { model
                        | project = newProject
                        , waitingForFiles = newWaitingForFiles
                    }
            in
            if Set.isEmpty newWaitingForFiles then
                compile model.outputFormat newProject

            else
                ( Compiling newModel
                , filesToBeRead
                    |> Set.toList
                    |> List.map Ports.readFile
                    |> Cmd.batch
                )


handleReadFileError : ErrorCode -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileError errorCode =
    handleError (IOError errorCode)


{-| We're done reading and parsing files. All the IO is done, now we can do
the rest synchronously!
-}
compile : String -> Project Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
compile format project =
    let
        _ =
            project.modules
                |> Dict.values
                |> List.map
                    (\module_ ->
                        Dict.values module_.declarations
                            |> List.map
                                (\decl ->
                                    decl.body
                                        |> Declaration.mapBody Frontend.unwrap
                                        |> Debug.log (decl.module_ ++ "." ++ decl.name)
                                )
                    )

        emitter =
            case format of
                "JSON" ->
                    EmitJson.emitProject

                _ ->
                    EmitJS.emitProject
    in
    Ok project
        |> Result.andThen Desugar.desugar
        |> Result.andThen InferTypes.inferTypes
        |> Result.map Optimize.optimize
        |> Result.andThen emitter
        |> Result.mapError CompilerError
        |> writeToFSAndExit


{-| We've got our output ready for writing to the filesystem!
(Well, that or an error somewhere along the process.)

Let's do that - report the error or write the output to a file.

-}
writeToFSAndExit :
    Result CLIError (Dict FilePath FileContents)
    -> ( Model Frontend.ProjectFields, Cmd Msg )
writeToFSAndExit result =
    case result of
        Ok outputFiles ->
            let
                outputCmds =
                    outputFiles
                        |> Dict.toList
                        |> List.map
                            (\( filePath, fileContents ) ->
                                Ports.writeToFile
                                    { filePath = filePath
                                    , fileContents = fileContents
                                    }
                            )
            in
            ( Finished
            , Cmd.batch
                (println "Compilation finished, writing output to `out.js`."
                    :: outputCmds
                )
            )

        Err error ->
            handleError error


{-| When an error happens, we bail out as early as possible.
Stop everything, abort mission, jump ship!

The `EncounteredError` model stops most everything (`update`, `subscriptions`).

-}
handleError : CLIError -> ( Model dontcare, Cmd Msg )
handleError error =
    ( EncounteredError
    , printlnStderr (errorToString error)
    )


log : Msg -> Msg
log msg =
    let
        string =
            case msg of
                ReadFileSuccess { filePath } ->
                    "ReadFileSuccess: " ++ filePath

                ReadFileError error ->
                    "ReadFileError: " ++ errorToString (IOError error)

        _ =
            Debug.log string ()
    in
    msg



-- ERRORS


type CLIError
    = FileNotInSourceDirectories FilePath
    | IOError ErrorCode
    | CompilerError Error


type ErrorCode
    = FileOrDirectoryNotFound FilePath
    | OtherErrorCode String


errorToString : CLIError -> String
errorToString error =
    case error of
        FileNotInSourceDirectories filePath ->
            "File `"
                ++ filePath
                ++ "` is not a part of the `sourceDirectories` in elm.json."

        IOError errorCode ->
            case errorCode of
                FileOrDirectoryNotFound filePath ->
                    "File or directory `" ++ filePath ++ "` not found."

                OtherErrorCode other ->
                    "Encountered error `" ++ other ++ "`."

        CompilerError compilerError ->
            Error.toString compilerError


parseErrorCode : { errorCode : String, filePath : FilePath } -> ErrorCode
parseErrorCode { errorCode, filePath } =
    case errorCode of
        "ENOENT" ->
            FileOrDirectoryNotFound filePath

        _ ->
            OtherErrorCode errorCode


{-| TODO maybe there should be a "Checks" module for checks across phases?
-}
checkModuleNameAndFilePath : { sourceDirectory : FilePath, filePath : FilePath } -> Module Frontend.LocatedExpr TypeAnnotation -> Result Error (Module Frontend.LocatedExpr TypeAnnotation)
checkModuleNameAndFilePath { sourceDirectory, filePath } ({ name } as parsedModule) =
    let
        expectedName : Result CLIError ModuleName
        expectedName =
            ModuleName.expectedModuleName
                { sourceDirectory = sourceDirectory
                , filePath = filePath
                }
                |> Result.fromMaybe (FileNotInSourceDirectories filePath)
    in
    if expectedName == Ok name then
        Ok parsedModule

    else
        Err
            (ParseError
                (ModuleNameDoesntMatchFilePath
                    { moduleName = name
                    , filePath = filePath
                    }
                )
            )
