module Error exposing
    ( EmitError(..)
    , Error(..)
    , OptimizeError(..)
    , ParseError(..)
    , TypeError(..)
    , toString
    )

import Common exposing (FilePath(..), ModuleName(..))
import Json.Decode as JD


type Error
    = ParseError ParseError
    | TypeError TypeError
    | OptimizeError OptimizeError
    | EmitError EmitError


type ParseError
    = ModuleNameDoesntMatchFileName ModuleName FilePath
    | FileNotFound FilePath
    | EmptySourceDirectories
    | MainModuleNotInSourceDirectory FilePath
    | InvalidElmJson JD.Error


{-| TODO
-}
type TypeError
    = TodoFirstTypeError


{-| TODO
-}
type OptimizeError
    = TodoFirstOptimizeError


{-| TODO
-}
type EmitError
    = TodoFirstEmitError


toString : Error -> String
toString error =
    case error of
        ParseError parseError ->
            case parseError of
                ModuleNameDoesntMatchFileName (ModuleName moduleName) (FilePath filePath) ->
                    "Module name `" ++ moduleName ++ "` doesn't match the file path `" ++ filePath ++ "`."

                FileNotFound (FilePath filePath) ->
                    "File `" ++ filePath ++ "` not found."

                EmptySourceDirectories ->
                    "Empty `sourceDirectories`!"

                MainModuleNotInSourceDirectory (FilePath filePath) ->
                    "The main module `" ++ filePath ++ "` given as argument is not located in the sourceDirectories."

                InvalidElmJson jsonError ->
                    "Invalid elm.json! " ++ JD.errorToString jsonError

        TypeError typeError ->
            Debug.todo "toString typeError"

        OptimizeError optimizeError ->
            Debug.todo "toString optimizeError"

        EmitError emitError ->
            Debug.todo "toString emitError"
