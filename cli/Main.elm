module Main exposing (main)

{-| Welcome to the Elm compiler written in Elm!

To get things out of the way: it would be great if the pipeline could be pure:

    input
        |> tokenize
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
import Dict.Extra
import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error as Error exposing (Error(..), LocatedParseErrorType(..), ParseError(..))
import Elm.Data.Declaration as Declaration
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath as FilePath exposing (FilePath)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName as ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (PossiblyQualified)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Project
import Json.Decode as JD
import OurExtras.Tuple3 as Tuple3
import Platform
import Ports exposing (println)
import Set exposing (Set)
import Stage.Desugar as Desugar
import Stage.Emit.HVM as EmitHVM
import Stage.Emit.JavaScript as EmitJS
import Stage.Emit.JsonAST as EmitJson
import Stage.Emit.Python as EmitPython
import Stage.InferTypes as InferTypes
import Stage.Optimize as Optimize
import Stage.Parse as Parse
import Stage.Parse.Lib as P
import Stage.Tokenize as Tokenize
import String.Extra as String


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
    , mainFileContents : String
    , elmJson : String
    , outputFormat : String
    }


{-| `Compiling` is the state we'll be most of the time. The other two are
mostly useless; they do effectively stop `subscriptions` and `update` though.
-}
type Model projectFields
    = Compiling (Model_ projectFields)
    | {- We don't need to remember the error, because we report it
         at the time of transition to this new model. See `handleFatalError`.
      -}
      EncounteredError
    | Finished


{-| Because we're mostly in the `Compiling` state, it is worthwhile
to make functions work with its data instead of the general `Model`.
-}
type alias Model_ projectFields =
    { project : Project projectFields
    , waitingForFiles : Dict ModuleName (Set FilePath)
    , outputFormat : String
    }


type Msg
    = ReadFileSuccess
        { filePath : FilePath
        , fileContents : FileContents
        }
    | ReadFileError
        { moduleName : ModuleName
        , filePath : FilePath
        , errorCode : String
        }


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
                ReadFileError
                ReadFileSuccess

        EncounteredError ->
            Sub.none

        Finished ->
            Sub.none


{-| The JS wrapper gives us the main filepath, its contents, and the contents
of the elm.json file.
We have two tasks here:

  - decode the elm.json file to something meaningful
  - find (and check against the filepath) the main module name

-}
init : Flags -> ( Model Frontend.ProjectFields, Cmd Msg )
init { mainFilePath, mainFileContents, elmJson, outputFormat } =
    let
        elmJsonProject : Result Error Elm.Project.Project
        elmJsonProject =
            JD.decodeString (JD.map normalizeDirs Elm.Project.decoder) elmJson
                |> Result.mapError (ParseError << InvalidElmJson)

        mainModuleName : Result CLIError ModuleName
        mainModuleName =
            findMainModuleName mainFilePath mainFileContents

        modelAndCmd : Result CLIError ( Model Frontend.ProjectFields, Cmd Msg )
        modelAndCmd =
            Result.map2
                (\mainModuleName_ elmJsonProject_ ->
                    let
                        model =
                            { project =
                                { mainFilePath = mainFilePath
                                , mainModuleName = mainModuleName_
                                , elmJson = elmJsonProject_
                                , sourceDirectories = getSourceDirectories elmJsonProject_
                                , modules = Dict.empty
                                }
                            , waitingForFiles = Dict.singleton mainModuleName_ (Set.singleton mainFilePath)
                            , outputFormat = outputFormat
                            }
                    in
                    model
                        |> handleReadFileSuccess
                            MainModule
                            { filePath = mainFilePath
                            , fileContents = mainFileContents
                            }
                )
                mainModuleName
                (elmJsonProject |> Result.mapError CompilerError)
    in
    case modelAndCmd of
        Ok modelAndCmd_ ->
            modelAndCmd_

        Err error ->
            handleFatalError error


{-| First, read the declared module name from the file contents.
Then, check it against the file path.

The funny thing is that the main file doesn't have to be present in the source
directories mentioned in elm.json, so we have to try and match a part of the
filename with the read module name and see if it matches.

We can be a little clever: count the dots in the supposedly correct module name
in the file contents, then to get the source directory, remove "that amount + 1"
of parts separated by `/` slashes in the filename.

Either this agrees or nothing will.

    module C.D.E exposing (..)
    A/B/C/D/E.elm
    -> 2 dots, remove 2+1 rightmost parts separated by `/` from the filepath
    -> remove 3 rightmost parts from [A,B,C,D,E.elm]
    -> [A,B]
    -> join using "/"
    -> `A/B`

-}
findMainModuleName : FilePath -> String -> Result CLIError ModuleName
findMainModuleName filePath contents =
    getDeclaredModuleName contents
        |> Result.andThen
            (\declaredModuleName ->
                let
                    numberOfDotsInModuleName : Int
                    numberOfDotsInModuleName =
                        String.countOccurrences "." declaredModuleName

                    sourceDirectory : String
                    sourceDirectory =
                        filePath
                            |> String.split "/"
                            |> List.reverse
                            |> List.drop (numberOfDotsInModuleName + 1)
                            |> List.reverse
                            |> String.join "/"
                in
                ModuleName.expectedModuleName
                    { sourceDirectory = sourceDirectory
                    , filePath = filePath
                    }
                    |> Result.fromMaybe
                        (ModuleNameDoesntMatchFilePath
                            { moduleName = declaredModuleName
                            , filePath = filePath
                            }
                            |> ParseError
                            |> CompilerError
                        )
            )


getDeclaredModuleName : String -> Result CLIError ModuleName
getDeclaredModuleName fileContents =
    fileContents
        |> String.leftOf "exposing"
        |> Tokenize.tokenize
        |> Result.andThen (P.run Parse.moduleName)
        |> Result.mapError CompilerError


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
getSourceDirectories : Elm.Project.Project -> List FilePath
getSourceDirectories elmProject =
    case elmProject of
        Elm.Project.Application { dirs } ->
            dirs

        Elm.Project.Package _ ->
            [ "src" ]


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
            handleReadFileSuccess OtherModule file model

        ReadFileError r ->
            handleReadFileError r model


type ModuleType
    = MainModule
    | OtherModule


handleReadFileSuccess :
    ModuleType
    -> { filePath : FilePath, fileContents : FileContents }
    -> Model_ Frontend.ProjectFields
    -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileSuccess moduleType ({ filePath } as file) ({ project } as model) =
    if isWaitingFor filePath model then
        let
            parseResult =
                Tokenize.tokenize file.fileContents
                    |> Result.andThen (Parse.parse file.filePath)
                    |> Debug.log "parsed"
                    |> Result.andThen
                        (case moduleType of
                            MainModule ->
                                {- We've already checked the main module name in a
                                   bit different way during `init`. (Differently
                                   because main modules don't need to live inside
                                   the source directories.) No need to check
                                   it again!
                                -}
                                Ok

                            OtherModule ->
                                checkModuleNameAndFilePath
                                    { sourceDirectories = project.sourceDirectories
                                    , filePath = filePath
                                    }
                        )
        in
        case parseResult of
            Err error ->
                handleFatalError <| CompilerError error

            Ok ({ name, imports } as parsedModule) ->
                let
                    newModules : Dict ModuleName (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
                    newModules =
                        Dict.update name
                            (always (Just parsedModule))
                            project.modules

                    newProject : Project Frontend.ProjectFields
                    newProject =
                        { project | modules = newModules }

                    filesToBeRead : Dict ModuleName (Set FilePath)
                    filesToBeRead =
                        imports
                            |> Dict.filter (\moduleName _ -> not <| Dict.member moduleName newModules)
                            |> Dict.keys
                            |> List.map
                                (\moduleName ->
                                    {- We don't know in which source directory the
                                       module will be, so we're trying them all.

                                       Our Model_.waitingForFiles remembers all the
                                       tries and:
                                         * when one succeeds the others are forgotten
                                         * when all fail we raise an error
                                    -}
                                    ( moduleName
                                    , project.sourceDirectories
                                        |> List.map
                                            (\sourceDirectory ->
                                                FilePath.expectedFilePath
                                                    { sourceDirectory = sourceDirectory
                                                    , moduleName = moduleName
                                                    }
                                            )
                                        |> Set.fromList
                                    )
                                )
                            |> Dict.fromList

                    newWaitingForFiles : Dict ModuleName (Set FilePath)
                    newWaitingForFiles =
                        model.waitingForFiles
                            |> Dict.remove name
                            {- There's no need for a more general Dict.merge
                               here because a module name is always going to
                               generate the same set of possible file paths over
                               the run of the compiler.
                            -}
                            |> Dict.union filesToBeRead

                    newModel : Model_ Frontend.ProjectFields
                    newModel =
                        { model
                            | project = newProject
                            , waitingForFiles = newWaitingForFiles
                        }
                in
                if Dict.isEmpty newWaitingForFiles then
                    compile model.outputFormat newProject

                else
                    ( Compiling newModel
                    , filesToBeRead
                        |> Dict.toList
                        |> List.concatMap
                            (\( moduleName, possibleFiles ) ->
                                possibleFiles
                                    |> Set.toList
                                    |> List.map
                                        (\filePath_ ->
                                            { moduleName = moduleName
                                            , filePath = filePath_
                                            }
                                        )
                            )
                        |> List.map Ports.readFile
                        |> Cmd.batch
                    )

    else
        handleFatalError (MultipleFilesForModule filePath)


isWaitingFor : FilePath -> Model_ projectFields -> Bool
isWaitingFor filePath { waitingForFiles } =
    Dict.Extra.any
        (\_ possibleFiles -> Set.member filePath possibleFiles)
        waitingForFiles


handleReadFileError :
    { moduleName : ModuleName
    , filePath : FilePath
    , errorCode : String
    }
    -> Model_ Frontend.ProjectFields
    -> ( Model Frontend.ProjectFields, Cmd Msg )
handleReadFileError { moduleName, filePath } model =
    case Dict.get moduleName model.waitingForFiles of
        Nothing ->
            {- We've got a read error after a file was successfully found
               elsewhere; we can safely ignore it!
            -}
            ( Compiling model
            , Cmd.none
            )

        Just possibleFiles ->
            if Set.isEmpty (Set.remove filePath possibleFiles) then
                -- This was our last chance for finding that module!
                handleFatalError (ModuleNotInSourceDirectories moduleName)

            else
                ( Compiling
                    { model
                        | waitingForFiles =
                            model.waitingForFiles
                                |> Dict.update moduleName
                                    {- Due to the above `Set.isEmpty` check we know that
                                       removing this possibility leaves some more
                                       possibilities open; we won't end up with an empty
                                       set then.
                                    -}
                                    (Maybe.map (Set.remove filePath))
                    }
                , Cmd.none
                )


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
                                        |> Declaration.mapBody
                                            Frontend.unwrap
                                            identity
                                            identity
                                        |> Debug.log (decl.module_ ++ "." ++ decl.name)
                                )
                    )

        emitter =
            case format of
                "json" ->
                    EmitJson.emitProject

                "python" ->
                    EmitPython.emitProject

                "js" ->
                    EmitJS.emitProject

                "hvm" ->
                    EmitHVM.emitProject

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
                (println "Compilation finished, writing output."
                    :: outputCmds
                )
            )

        Err error ->
            handleFatalError error


{-| When an error happens, we bail out as early as possible.
Stop everything, abort mission, jump ship!

The `EncounteredError` model stops most everything (`update`, `subscriptions`).

-}
handleFatalError : CLIError -> ( Model dontcare, Cmd Msg )
handleFatalError error =
    ( EncounteredError
    , Cmd.batch
        [ Ports.printlnStderr (errorToString error)
        , Ports.setExitCode 1
        ]
    )


log : Msg -> Msg
log msg =
    let
        string =
            case msg of
                ReadFileSuccess { filePath } ->
                    "ReadFileSuccess: " ++ filePath

                ReadFileError error ->
                    "ReadFileError: " ++ Debug.toString error

        _ =
            Debug.log string ()
    in
    msg



-- ERRORS


type CLIError
    = ModuleNotInSourceDirectories ModuleName
    | MultipleFilesForModule FilePath
    | CompilerError Error


errorToString : CLIError -> String
errorToString error =
    case error of
        ModuleNotInSourceDirectories moduleName ->
            "Module `"
                ++ moduleName
                ++ "` cannot be found in your `sourceDirectories`."

        MultipleFilesForModule filePath ->
            {- TODO better error by tracking both the previously successful file
               and this one, and the module name.
            -}
            "File `"
                ++ filePath
                ++ "` clashes with some other module in your `sourceDirectories`."

        CompilerError compilerError ->
            Error.toString compilerError


{-| TODO maybe there should be a "Checks" module for checks across phases?
-}
checkModuleNameAndFilePath :
    { sourceDirectories : List FilePath, filePath : FilePath }
    -> Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified
    -> Result Error (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
checkModuleNameAndFilePath { sourceDirectories, filePath } ({ name } as parsedModule) =
    let
        makesNameAgree : FilePath -> Bool
        makesNameAgree sourceDirectory =
            ModuleName.expectedModuleName
                { sourceDirectory = sourceDirectory
                , filePath = filePath
                }
                == Just name
    in
    if List.any makesNameAgree sourceDirectories then
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
