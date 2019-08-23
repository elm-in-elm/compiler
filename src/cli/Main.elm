module Main exposing (main)

{-| Welcome to the Elm compiler written in Elm!

To get things out of the way: it would be great if the pipeline could be pure:

    input
        |> parse
        |> desugar
        |> inferTypes
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

-}

import AST.Frontend as Frontend
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Data.Declaration as Declaration
import Data.FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Module exposing (Modules)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.Project exposing (Project)
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
import Stage.Desugar as Desugar
import Stage.Emit.JavaScript as EmitJS
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
    }


type Msg
    = ReadFileSuccess FilePath FileContents
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
init { mainFilePath, elmJson } =
    let
        mainFilePath_ : FilePath
        mainFilePath_ =
            FilePath.fromString mainFilePath

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
                        ModuleName.expected
                            { sourceDirectory = FilePath.toString sourceDirectory_
                            , filePath = FilePath.toString mainFilePath_
                            }
                            -- TODO this conversion to Result is duplicated. We should really return the Result Error ... in the Name.expectedModuleName!
                            |> Result.fromMaybe (GeneralError <| FileNotInSourceDirectories mainFilePath_)
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
                            , modules = Dict.empty
                            }
                        , waitingForFiles = Set.singleton mainFilePath_
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
                |> {- TODO allow multiple source directories -} List.head
                |> Maybe.map FilePath.fromString
                |> Result.fromMaybe (ParseError EmptySourceDirectories)

        Elm.Project.Package _ ->
            -- TODO is it OK that this has the trailing slash?
            Ok <| FilePath.fromString "src/"


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
        ReadFileSuccess filePath fileContents ->
            handleReadFileSuccess filePath fileContents model

        ReadFileError errorCode ->
            handleReadFileError errorCode


handleReadFileSuccess : FilePath -> FileContents -> Model_ Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileSuccess filePath fileContents ({ project } as model) =
    let
        parseResult =
            Parse.parse filePath fileContents
                |> Result.andThen
                    (Parse.checkModuleNameAndFilePath
                        { sourceDirectory = project.sourceDirectory
                        , filePath = filePath
                        }
                    )
    in
    case parseResult of
        Err error ->
            handleError error

        Ok ({ name, imports } as parsedModule) ->
            let
                filesToBeRead : Set FilePath
                filesToBeRead =
                    imports
                        |> Dict.keys
                        |> List.map (FilePath.expected project.sourceDirectory)
                        |> Set.fromList

                newModules : Modules Frontend.LocatedExpr
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
                compile newProject

            else
                ( Compiling newModel
                , filesToBeRead
                    |> Set.toList
                    |> List.map Ports.readFile
                    |> Cmd.batch
                )


handleReadFileError : ErrorCode -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileError errorCode =
    handleError (GeneralError (IOError errorCode))


{-| We're done reading and parsing files. All the IO is done, now we can do
the rest synchronously!
-}
compile : Project Frontend.ProjectFields -> ( Model Frontend.ProjectFields, Cmd Msg )
compile project =
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
                                        |> Debug.log (Declaration.toString decl)
                                )
                    )
    in
    Ok project
        |> Result.andThen Desugar.desugar
        |> Result.andThen InferTypes.inferTypes
        |> Result.map Optimize.optimize
        |> Result.andThen EmitJS.emitProject
        |> writeToFSAndExit


{-| We've got our output ready for writing to the filesystem!
(Well, that or an error somewhere along the process.)

Let's do that - report the error or write the output to a file.

-}
writeToFSAndExit : Result Error (Dict FilePath FileContents) -> ( Model Frontend.ProjectFields, Cmd Msg )
writeToFSAndExit result =
    case result of
        Ok outputFiles ->
            let
                outputCmds =
                    outputFiles
                        |> Dict.toList
                        |> List.map (\( filePath, fileContents ) -> Ports.writeToFile filePath fileContents)
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
                ReadFileSuccess filePath _ ->
                    "ReadFileSuccess: " ++ FilePath.toString filePath

                ReadFileError error ->
                    "ReadFileError: " ++ Error.toString (GeneralError (IOError error))

        _ =
            Debug.log string ()
    in
    msg
