module Error exposing
    ( DesugarError(..)
    , EmitError(..)
    , Error(..)
    , ErrorCode(..)
    , GeneralError(..)
    , OptimizeError(..)
    , ParseContext(..)
    , ParseError(..)
    , ParseProblem(..)
    , PrepareForBackendError(..)
    , TypeError(..)
    , parseErrorCode
    , toString
    )

import AST.Common.Type as Type exposing (Type)
import Common.Types
    exposing
        ( FilePath(..)
          -- TODO we can't depend on Common, so we deconstruct by hand
        , ModuleName(..)
        , VarName(..)
        )
import Json.Decode as JD
import Parser.Advanced as P


type Error
    = GeneralError GeneralError
    | ParseError ParseError
    | DesugarError DesugarError
    | TypeError TypeError
    | OptimizeError OptimizeError
    | PrepareForBackendError PrepareForBackendError
    | EmitError EmitError


type GeneralError
    = FileNotInSourceDirectories FilePath
    | IOError ErrorCode


type ParseError
    = ModuleNameDoesntMatchFilePath ModuleName FilePath
    | FileNotFound FilePath
    | EmptySourceDirectories
    | InvalidElmJson JD.Error
    | ParseProblem (List (P.DeadEnd ParseContext ParseProblem))


type ParseContext
    = InLiteral
    | InLiteralInt
    | InExpr


type ParseProblem
    = ExpectingPortKeyword -- `>port< module ...`
    | ExpectingEffectKeyword -- `>effect< module ...`
    | ExpectingModuleKeyword -- `>module< Foo.Bar exposing (..)`
    | ExpectingModuleName -- `module >Foo.Bar< exposing (..)`
    | ExpectingExposingKeyword -- `module Foo.Bar >exposing< (..)`
    | ExpectingExposingAllSymbol -- `module Foo.Bar exposing >(..)<`
    | ExpectingExposingListLeftParen -- `module Foo.Bar exposing >(<a, b, c)`
    | ExpectingExposingListRightParen -- `module Foo.Bar exposing (a, b, c>)<`
    | ExpectingExposingListSeparatorComma -- `module Foo.Bar exposing (a>,< b, c)`
    | ExpectingExposedTypeDoublePeriod -- `module Foo.Bar exposing (Foo>(..)<)`
    | ExpectingVarName -- eg. `module Foo.Bar exposing (>a<)`
    | ExpectingTypeOrConstructorName -- eg. `module Foo.Bar exposing (>Foo<)`
    | ExposingListCantBeEmpty -- `module Foo.Bar exposing >()<`
    | ExpectingImportKeyword -- `>import< Foo as F exposing (..)`
    | ExpectingAsKeyword -- `import Foo >as< F exposing (..)`
    | ExpectingModuleNameWithoutDots -- `import Foo as >F< exposing (..)`
    | ExpectingModuleNamePart -- `Foo.>Bar<.Baz.value`
    | ExpectingQualifiedVarNameDot -- `x = Foo>.<y`
    | ExpectingEqualsSign -- `x >=< 1`
    | ExpectingMinusSign -- `>-<42`
    | ExpectingInt
    | ExpectingSingleQuote
    | ExpectingChar
    | ExpectingDoubleQuote
    | ExpectingPlusOperator
    | ExpectingModuleDot -- `import Foo>.<Bar`
    | ExpectingBackslash -- `>\<x -> x + 1`
    | ExpectingRightArrow -- `\x >->< x + 1`
    | ExpectingLeftParen
    | ExpectingRightParen
    | ExpectingNotBeginningOfLine
    | ExpectingIf
    | ExpectingThen
    | ExpectingElse
    | ExpectingTrue
    | ExpectingFalse
    | InvalidInt
    | CompilerBug String


{-| TODO records are probably better for communicating the meaning of args.
-}
type DesugarError
    = VarNotInEnvOfModule (Maybe ModuleName) VarName ModuleName
    | AmbiguousVar (Maybe ModuleName) VarName ModuleName


{-| TODO annotate all type errors so that we can show the position in source code!
-}
type TypeError
    = UnknownName VarName
    | TypeMismatch Type Type
      -- TODO explain what "occurs check" is
    | OccursCheckFailed Int Type


{-| TODO
-}
type OptimizeError
    = TodoFirstOptimizeError


type PrepareForBackendError
    = MainDeclarationNotFound


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

                IOError errorCode ->
                    case errorCode of
                        FileOrDirectoryNotFound (FilePath filePath) ->
                            "File or directory `" ++ filePath ++ "` not found."

                        OtherErrorCode other ->
                            "Encountered error `" ++ other ++ "`."

        ParseError parseError ->
            case parseError of
                ModuleNameDoesntMatchFilePath (ModuleName moduleName) (FilePath filePath) ->
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

                ParseProblem problems ->
                    String.join "\n"
                        ("Parse problems: "
                            :: List.map (\problem -> "  " ++ Debug.toString problem) problems
                        )

        DesugarError desugarError ->
            case desugarError of
                VarNotInEnvOfModule maybeModuleName varName (ModuleName moduleName) ->
                    "Can't find the variable `"
                        ++ fullVarName maybeModuleName varName
                        ++ "` in the module `"
                        ++ moduleName
                        ++ "`. Have you imported it?"

                AmbiguousVar maybeModuleName varName (ModuleName moduleName) ->
                    "There are multiple definitions for variable `"
                        ++ fullVarName maybeModuleName varName
                        ++ "` in the module `"
                        ++ moduleName
                        ++ "`. Keep only one in the code! Maybe alias some imports to fix the collision?"

        TypeError typeError ->
            case typeError of
                UnknownName (VarName varName) ->
                    "I've encountered a variable name I haven't seen before while typechecking your program: "
                        ++ varName

                TypeMismatch t1 t2 ->
                    "The types "
                        ++ Type.toString t1
                        ++ " and "
                        ++ Type.toString t2
                        ++ " don't match."

                OccursCheckFailed varId type_ ->
                    -- TODO better error. Is this cycle? Infinite type?
                    "An \"occurs check\" failed while typechecking: "
                        ++ Type.toString (Type.Var varId)
                        ++ " occurs in "
                        ++ Type.toString type_

        OptimizeError _ ->
            Debug.todo "toString optimizeError"

        PrepareForBackendError prepareForBackendError ->
            case prepareForBackendError of
                MainDeclarationNotFound ->
                    "Couldn't find the value `main` in the main module given to the compiler!"

        EmitError _ ->
            Debug.todo "toString emitBackendError"


fullVarName : Maybe ModuleName -> VarName -> String
fullVarName maybeModuleAlias (VarName varName) =
    maybeModuleAlias
        |> Maybe.map (\(ModuleName moduleAlias) -> moduleAlias ++ "." ++ varName)
        |> Maybe.withDefault varName


parseErrorCode : String -> FilePath -> ErrorCode
parseErrorCode code filePath =
    case code of
        "ENOENT" ->
            FileOrDirectoryNotFound filePath

        _ ->
            OtherErrorCode code


type ErrorCode
    = FileOrDirectoryNotFound FilePath
    | OtherErrorCode String
