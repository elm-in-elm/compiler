module Error exposing
    ( DesugarError(..)
    , EmitError(..)
    , Error(..)
    , ErrorCode(..)
    , GeneralError(..)
    , ParseContext(..)
    , ParseError(..)
    , ParseProblem(..)
    , TypeError(..)
    , parseErrorCode
    , toString
    )

import AST.Common.Type as Type exposing (Type)
import Data.FilePath as FilePath exposing (FilePath)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)
import Json.Decode as JD
import Parser.Advanced as P


type Error
    = GeneralError GeneralError
    | ParseError ParseError
    | DesugarError DesugarError
    | TypeError TypeError
    | EmitError EmitError


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
    | InTuple
    | InTuple3


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
    | ExpectingConsOperator
    | ExpectingConcatOperator
    | ExpectingModuleDot -- `import Foo>.<Bar`
    | ExpectingBackslash -- `>\<x -> x + 1`
    | ExpectingRightArrow -- `\x >->< x + 1`
    | ExpectingLeftParen
    | ExpectingRightParen
    | ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingListSeparator
    | ExpectingTupleSeparator
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


type EmitError
    = MainDeclarationNotFound


toString : Error -> String
toString error =
    case error of
        GeneralError generalError ->
            case generalError of
                FileNotInSourceDirectories filePath ->
                    "File `"
                        ++ FilePath.toString filePath
                        ++ "` is not a part of the `sourceDirectories` in elm.json."

                IOError errorCode ->
                    case errorCode of
                        FileOrDirectoryNotFound filePath ->
                            "File or directory `" ++ FilePath.toString filePath ++ "` not found."

                        OtherErrorCode other ->
                            "Encountered error `" ++ other ++ "`."

        ParseError parseError ->
            case parseError of
                ModuleNameDoesntMatchFilePath moduleName filePath ->
                    "Module name `"
                        ++ ModuleName.toString moduleName
                        ++ "` doesn't match the file path `"
                        ++ FilePath.toString filePath
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

                        moduleName =
                            ModuleName.toString module_
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

        EmitError emitError ->
            case emitError of
                MainDeclarationNotFound ->
                    "Couldn't find the value `main` in the main module given to the compiler!"


fullVarName : Maybe ModuleName -> VarName -> String
fullVarName maybeModuleAlias varName =
    let
        varName_ =
            VarName.toString varName
    in
    maybeModuleAlias
        |> Maybe.map (\moduleAlias -> ModuleName.toString moduleAlias ++ "." ++ varName_)
        |> Maybe.withDefault varName_


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
