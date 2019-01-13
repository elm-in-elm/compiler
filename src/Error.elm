module Error exposing
    ( DesugarError(..)
    , EmitError(..)
    , Error(..)
    , GeneralError(..)
    , OptimizeError(..)
    , ParseError(..)
    , TypeError(..)
    , toString
    )

import Common.Types exposing (FilePath(..), ModuleName(..))
import Json.Decode as JD


type Error
    = GeneralError GeneralError
    | ParseError ParseError
    | DesugarError DesugarError
    | TypeError TypeError
    | OptimizeError OptimizeError
    | EmitError EmitError


type GeneralError
    = FileNotInSourceDirectories FilePath


type ParseError
    = ModuleNameDoesntMatchFileName ModuleName FilePath
    | FileNotFound FilePath
    | EmptySourceDirectories
    | InvalidElmJson JD.Error


{-| TODO
-}
type TypeError
    = TodoFirstTypeError


{-| TODO
-}
type DesugarError
    = TodoFirstDesugarError


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
        GeneralError generalError ->
            case generalError of
                FileNotInSourceDirectories (FilePath filePath) ->
                    "File `"
                        ++ filePath
                        ++ "` is not a part of the `sourceDirectories` in elm.json."

        ParseError parseError ->
            case parseError of
                ModuleNameDoesntMatchFileName (ModuleName moduleName) (FilePath filePath) ->
                    "Module name `"
                        ++ moduleName
                        ++ "` doesn't match the file path `"
                        ++ filePath
                        ++ "`."

                FileNotFound (FilePath filePath) ->
                    "File `"
                        ++ filePath
                        ++ "` not found."

                EmptySourceDirectories ->
                    "Empty `sourceDirectories`!"

                InvalidElmJson jsonError ->
                    "Invalid elm.json! "
                        ++ JD.errorToString jsonError

        DesugarError desugarError ->
            Debug.todo "toString desugarError"

        TypeError typeError ->
            Debug.todo "toString typeError"

        OptimizeError optimizeError ->
            Debug.todo "toString optimizeError"

        EmitError emitError ->
            Debug.todo "toString emitError"
