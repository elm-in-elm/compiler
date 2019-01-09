module Main exposing (main)

import Common exposing (FileContents(..))
import Dict exposing (Dict)
import Dict.Extra as Dict
import Platform
import Ports
    exposing
        ( println
        , printlnStderr
        , readFile
        , waitForReadFile
        , writeToFile
        )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { -- TODO allow for multiple `main`s instead of just one
      mainFilepath : String
    }


type alias Model =
    { flags : Flags
    , files : Dict String FileStage
    }


type Msg
    = -- TODO ReadFileFailure
      ReadFileSuccess String FileContents


type FileStage
    = ToBeRead
    | DoneParsing
        { sourceCode : FileContents
        , ast : AST
        }
    | DoneTypechecking AST


type AST
    = -- TODO
      TodoDefineAST


type ModuleName
    = ModuleName String


type TypeError
    = -- TODO
      TodoDefineTypeError


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , files = Dict.singleton flags.mainFilepath ToBeRead
      }
    , readFile flags.mainFilepath
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
    case log msg of
        ReadFileSuccess fileName fileContents ->
            model
                |> parseFile fileName fileContents


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


parseFile : String -> FileContents -> Model -> ( Model, Cmd Msg )
parseFile fileName fileContents model =
    let
        ast : AST
        ast =
            Debug.todo "ast"

        newFilesToBeRead : List String
        newFilesToBeRead =
            ast
                |> findModuleNames
                |> List.map moduleToFilepath

        filesWithReadFile : Dict String FileStage
        filesWithReadFile =
            model.files
                |> Dict.update fileName
                    (Maybe.map
                        (always
                            (DoneParsing
                                { sourceCode = fileContents
                                , ast = ast
                                }
                            )
                        )
                    )

        newFiles : Dict String FileStage
        newFiles =
            newFilesToBeRead
                |> List.map (\filename -> ( filename, ToBeRead ))
                |> Dict.fromList
                |> Dict.union
                    -- it's important that this dict is the first argument to Dict.union
                    -- because in case of conflict it (already read file) is used
                    -- instead of the `ToBeRead` one
                    filesWithReadFile

        newModel : Model
        newModel =
            { model | files = newFiles }
    in
    if Dict.any (\_ fileStage -> fileStage == ToBeRead) newFiles then
        ( newModel
        , newFilesToBeRead
            |> List.map readFile
            |> Cmd.batch
        )

    else
        -- TODO use `elm-continue` to make this pattern cleaner
        typecheck newModel


findModuleNames : AST -> List ModuleName
findModuleNames ast =
    Debug.todo "findModuleNames"


moduleToFilepath : ModuleName -> String
moduleToFilepath (ModuleName moduleName) =
    -- TODO make filepaths their own type (almost) everywhere?
    -- TODO un-hardcodize "src/"
    "src/" ++ String.replace "." "/" moduleName ++ ".elm"


typecheck : Model -> ( Model, Cmd Msg )
typecheck model =
    -- TODO actually do some typechecking
    let
        errors : List TypeError
        errors =
            []
    in
    if List.isEmpty errors then
        optimize
            { model
                | files =
                    model.files
                        |> Dict.map
                            (\_ fileStage ->
                                case fileStage of
                                    DoneParsing { ast } ->
                                        DoneTypechecking ast

                                    -- If any of these happen, something went wrong. We're supposed
                                    -- to read and parse everything, then typecheck everything,
                                    -- then optimize etc.
                                    ToBeRead ->
                                        fileStage
                                            |> Debug.log "something went wrong - `typecheck` encountered `ToBeRead` value"

                                    DoneTypechecking _ ->
                                        fileStage
                                            |> Debug.log "something went wrong - `typecheck` encountered `DoneTypechecking` value"
                            )
            }

    else
        ( model
        , printlnStderr ("Errors! " ++ Debug.toString errors)
        )


optimize : Model -> ( Model, Cmd Msg )
optimize model =
    -- TODO actually do some optimizing
    let
        newFiles =
            model.files
    in
    emitJS { model | files = newFiles }


emitJS : Model -> ( Model, Cmd Msg )
emitJS model =
    let
        emittedJS : String
        emittedJS =
            -- TODO something real
            "console.log('boo');"
    in
    ( model
    , Cmd.batch
        [ writeToFile "out.js" (FileContents emittedJS)
        , println "Successfully compiled to (hardcoded) out.js!"
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if anyFilesToBeRead model then
        waitForReadFile ReadFileSuccess

    else
        Sub.none


anyFilesToBeRead : Model -> Bool
anyFilesToBeRead { files } =
    Dict.any (\_ fileStage -> fileStage == ToBeRead) files
