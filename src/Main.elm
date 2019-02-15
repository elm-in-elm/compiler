module Main exposing (main)

{-| Welcome to the Elm compiler written in Elm!

To get things out of the way: it would be great if the pipeline could be pure:

    input
        |> parse
        |> desugar
        |> typecheck
        |> optimize
        |> prepareForBackend
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

TODO downloading packages from the Internet (oh my!)

-}

import AST.Frontend as Frontend
import Common
import Common.Types
    exposing
        ( Dict_
        , FileContents(..)
        , FilePath(..)
        , Module
        , ModuleName(..)
        , Modules
        , Project
        , ProjectToEmit
        , Set_
        )
import Dict.Any exposing (AnyDict)
import Elm.Project
import Error
    exposing
        ( Error(..)
        , ErrorCode
        , GeneralError(..)
        , ParseError(..)
        )
import Json.Decode as JD
import Platform
import Ports exposing (println, printlnStderr)
import Set.Any exposing (AnySet)
import Stage.Desugar as Desugar
import Stage.Emit as Emit
import Stage.Optimize as Optimize
import Stage.Parse as Parse
import Stage.PrepareForBackend as PrepareForBackend
import Stage.Typecheck as Typecheck


{-| We're essentially a Node.JS app (until we get self-hosting :P ).
So, `Platform.worker` is the only option for us.

TODO expose parts of the compiler as a library!

-}
main : Program Flags (Model Frontend.ProjectFields) Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { mainFilePath : String -- TODO allow for multiple `main`s instead of just one
    , elmJson : String
    }


{-| `Compiling` is the state we'll be most of the time. The other two are
mostly useless; they do effectively stop `subscriptions` and `update` though.
-}
type Model expr
    = Compiling (Model_ expr)
    | {- We don't need to remember the error, because we report it
         at the time of transition to this new model. See `handleError`.
      -}
      EncounteredError
    | Finished


{-| Because we're mostly in the `Compiling` state, it is worthwhile
to make functions work with its data instead of the general `Model`.
-}
type alias Model_ expr =
    { project : Project expr
    , waitingForFiles : Set_ FilePath
    }


type Msg
    = ReadFileSuccess FilePath FileContents
    | ReadFileError FilePath ErrorCode


subscriptions : Model expr -> Sub Msg
subscriptions model =
    case model of
        Compiling model_ ->
            {- We'll be waiting for the various file contents we've asked for
               with `readFile`, but only on the happy path. They are of no use
               to us when we've already found an error elsewhere or finished
               the compilation.
            -}
            Ports.waitForReadFile ReadFileError ReadFileSuccess

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
init ({ mainFilePath, elmJson } as flags) =
    let
        mainFilePath_ : FilePath
        mainFilePath_ =
            FilePath mainFilePath

        elmJsonProject : Result Error Elm.Project.Project
        elmJsonProject =
            JD.decodeString (JD.map normalizeDirs Elm.Project.decoder) elmJson
                |> Result.mapError (ParseError << InvalidElmJson)

        sourceDirectory : Result Error FilePath
        sourceDirectory =
            elmJsonProject
                |> Result.andThen getSourceDirectory

        mainModuleName : Result Error ModuleName
        mainModuleName =
            sourceDirectory
                |> Result.andThen
                    (\sourceDirectory_ ->
                        {- TODO It is probably not enforced by the official compiler
                           for the main module's path to be included in the source directories.
                           We'd have to read the module name from the file contents in that case.
                           Check that assumption and do the right thing!
                        -}
                        Common.expectedModuleName sourceDirectory_ mainFilePath_
                    )

        modelAndCmd : Result Error ( Model Frontend.ProjectFields, Cmd Msg )
        modelAndCmd =
            Result.map3
                (\mainModuleName_ elmJsonProject_ sourceDirectory_ ->
                    ( Compiling
                        { project =
                            { mainFilePath = mainFilePath_
                            , mainModuleName = mainModuleName_
                            , elmJson = elmJsonProject_
                            , sourceDirectory = sourceDirectory_
                            , program = Dict.Any.empty Common.moduleNameToString
                            }
                        , waitingForFiles = Set.Any.singleton mainFilePath_ Common.filePathToString
                        }
                    , Ports.readFile mainFilePath_
                    )
                )
                mainModuleName
                elmJsonProject
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
            Elm.Project.Application { data | dirs = List.map normalizeDir data.dirs }

        Elm.Project.Package data ->
            Elm.Project.Package data


normalizeDir : String -> String
normalizeDir dir =
    if String.endsWith "/" dir then
        String.dropRight 1 dir

    else
        dir


{-| Applications tell us their source directories; packages have `src/`.
-}
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
    case log msg of
        ReadFileSuccess filePath fileContents ->
            handleReadFileSuccess filePath fileContents model

        ReadFileError filePath errorCode ->
            handleReadFileError filePath errorCode model


handleReadFileSuccess : FilePath -> FileContents -> Model_ Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileSuccess filePath fileContents ({ project } as model) =
    case Parse.parse project filePath fileContents of
        Err error ->
            handleError error

        Ok ({ name, dependencies } as parsedModule) ->
            let
                filesToBeRead : Set_ FilePath
                filesToBeRead =
                    dependencies
                        |> Dict.Any.keys
                        |> List.map (Common.expectedFilePath project.sourceDirectory)
                        |> Set.Any.fromList Common.filePathToString

                newProgram : Modules Frontend.Expr
                newProgram =
                    Dict.Any.update name
                        (always (Just parsedModule))
                        project.program

                newProject : Project Frontend.ProjectFields
                newProject =
                    { project | program = newProgram }

                newWaitingForFiles : Set_ FilePath
                newWaitingForFiles =
                    model.waitingForFiles
                        |> Set.Any.union filesToBeRead
                        |> Set.Any.remove filePath

                newModel : Model_ Frontend.ProjectFields
                newModel =
                    { model
                        | project = newProject
                        , waitingForFiles = newWaitingForFiles
                    }
            in
            if Set.Any.isEmpty newWaitingForFiles then
                compile newProject

            else
                ( Compiling newModel
                , filesToBeRead
                    |> Set.Any.toList
                    |> List.map Ports.readFile
                    |> Cmd.batch
                )


handleReadFileError : FilePath -> ErrorCode -> Model_ Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileError (FilePath filePath) errorCode model =
    handleError (GeneralError (IOError errorCode))


{-| We're done reading and parsing files. Now we can do the rest synchronously!
-}
compile : Project Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
compile project =
    Ok project
        |> Debug.log "after parse"
        |> Result.andThen Desugar.desugar
        |> Result.andThen Typecheck.typecheck
        |> Result.andThen Optimize.optimize
        |> Result.andThen PrepareForBackend.prepareForBackend
        |> Result.andThen Emit.emit
        |> Debug.log "after emit"
        |> finish


{-| We've got our output ready for writing to the filesystem!
(Well, that or an error somewhere along the process.)

Let's do that - report the error or write the output to a file.

-}
finish : Result Error ProjectToEmit -> ( Model Frontend.ProjectFields, Cmd Msg )
finish result =
    case result of
        Ok { output } ->
            ( Finished
            , Cmd.batch
                -- TODO don't hardcode out.js
                [ Ports.writeToFile (FilePath "out.js") output
                , println "Compilation finished, wrote output to `out.js`."
                ]
            )

        Err error ->
            handleError error


{-| When an error happens, we bail out as early as possible.
Stop everything, abort mission, jump ship!

The `EncounteredError` model stops most everything (`update`, `subscriptions`).

-}
handleError : Error -> ( Model dontcare, Cmd Msg )
handleError error =
    ( EncounteredError
    , printlnStderr (Error.toString error)
    )


log : Msg -> Msg
log msg =
    let
        string =
            case msg of
                ReadFileSuccess (FilePath filePath) _ ->
                    "ReadFileSuccess: " ++ filePath

                ReadFileError (FilePath filePath) _ ->
                    "ReadFileError: " ++ filePath

        _ =
            Debug.log string ()
    in
    msg
