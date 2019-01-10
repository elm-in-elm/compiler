module Error exposing
    ( EmitError(..)
    , Error(..)
    , OptimizeError(..)
    , ParseError(..)
    , TypeError(..)
    , toString
    )

import Common exposing (FilePath(..), ModuleName(..))


type Error
    = ParseError ParseError
    | TypeError TypeError
    | OptimizeError OptimizeError
    | EmitError EmitError


type ParseError
    = ModuleNameDoesntMatchFileName ModuleName FilePath
    | FileNotFound FilePath


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

        TypeError typeError ->
            Debug.todo "toString typeError"

        OptimizeError optimizeError ->
            Debug.todo "toString optimizeError"

        EmitError emitError ->
            Debug.todo "toString emitError"
