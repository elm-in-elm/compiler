module Error exposing
    ( DesugarError(..)
    , Error(..)
    , ErrorCode(..)
    , GeneralError(..)
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
          -- We can't depend on Common, so we deconstruct by hand
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
    | PrepareForBackendError PrepareForBackendError


type GeneralError
    = FileNotInSourceDirectories FilePath
    | IOError ErrorCode


type ParseError
    = ModuleNameDoesntMatchFilePath ModuleName FilePath
    | EmptySourceDirectories
    | InvalidElmJson JD.Error
    | ParseProblem (List (P.DeadEnd ParseContext ParseProblem))


type ParseContext
    = InLiteral
    | InNumber
    | InChar
    | InCharEscapeMode
    | InUnicodeCharacter
    | InString
    | InDoubleQuoteString
    | InTripleQuoteString
    | InExpr
    | InIf
    | InLet
    | InLetBinding
    | InLambda
    | InList
    | InUnit


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
    | ExpectingNumber
    | ExpectingSingleQuote
    | ExpectingChar
    | ExpectingEscapeBackslash
    | ExpectingEscapeCharacter Char
    | ExpectingUnicodeEscapeLeftBrace
    | ExpectingUnicodeEscapeRightBrace
    | InvalidUnicodeCodePoint
    | ExpectingDoubleQuote
    | ExpectingTripleQuote
    | ExpectingPlusOperator
    | ExpectingConcatOperator
    | ExpectingModuleDot -- `import Foo>.<Bar`
    | ExpectingBackslash -- `>\<x -> x + 1`
    | ExpectingRightArrow -- `\x >->< x + 1`
    | ExpectingLeftParen
    | ExpectingRightParen
    | ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingListSeparator
    | ExpectingNotBeginningOfLine
    | ExpectingIf
    | ExpectingThen
    | ExpectingElse
    | ExpectingTrue
    | ExpectingFalse
    | ExpectingLet
    | ExpectingIn
    | ExpectingUnit
    | InvalidNumber
    | TriedToParseCharacterStoppingDelimiter
    | CompilerBug String


type DesugarError
    = VarNotInEnvOfModule
        { var : ( Maybe ModuleName, VarName )
        , module_ : ModuleName
        }


type TypeError
    = TypeMismatch Type Type
    | OccursCheckFailed Int Type


type PrepareForBackendError
    = MainDeclarationNotFound


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
                VarNotInEnvOfModule { var, module_ } ->
                    let
                        ( maybeModuleName, varName ) =
                            var

                        (ModuleName moduleName) =
                            module_
                    in
                    "Can't find the variable `"
                        ++ fullVarName maybeModuleName varName
                        ++ "` in the module `"
                        ++ moduleName
                        ++ "`. Have you imported it?"

        TypeError typeError ->
            case typeError of
                TypeMismatch t1 t2 ->
                    let
                        -- Share index between types
                        ( type1, state1 ) =
                            Type.toString Type.emptyState t1

                        ( type2, _ ) =
                            Type.toString state1 t2
                    in
                    "The types `"
                        ++ type1
                        ++ "` and `"
                        ++ type2
                        ++ "` don't match."

                OccursCheckFailed varId type_ ->
                    let
                        ( type1, state1 ) =
                            Type.toString Type.emptyState (Type.Var varId)

                        ( type2, _ ) =
                            Type.toString state1 type_
                    in
                    "An \"occurs check\" failed while typechecking: "
                        ++ type1
                        ++ " occurs in "
                        ++ type2

        PrepareForBackendError prepareForBackendError ->
            case prepareForBackendError of
                MainDeclarationNotFound ->
                    "Couldn't find the value `main` in the main module given to the compiler!"


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
