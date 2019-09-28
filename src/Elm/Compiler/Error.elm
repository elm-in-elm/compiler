module Elm.Compiler.Error exposing
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

import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type as Type exposing (Type)
import Elm.Data.Type.ToString as TypeToString
import Elm.Data.VarName exposing (VarName)
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
    = ModuleNameDoesntMatchFilePath
        { moduleName : ModuleName
        , filePath : FilePath
        }
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
        { var : { module_ : Maybe ModuleName, name : VarName }
        , module_ : ModuleName
        }


type TypeError
    = TypeMismatch Type Type
    | OccursCheckFailed Int Type


type EmitError
    = MainDeclarationNotFound
    | ModuleNotFoundForVar { module_ : ModuleName, var : VarName }
    | ModuleNotFoundForType { module_ : ModuleName, type_ : VarName }
    | DeclarationNotFound { module_ : ModuleName, name : VarName }


toString : Error -> String
toString error =
    case error of
        GeneralError generalError ->
            case generalError of
                FileNotInSourceDirectories filePath ->
                    "File `"
                        ++ filePath
                        ++ "` is not a part of the `sourceDirectories` in elm.json."

                IOError errorCode ->
                    case errorCode of
                        FileOrDirectoryNotFound filePath ->
                            "File or directory `" ++ filePath ++ "` not found."

                        OtherErrorCode other ->
                            "Encountered error `" ++ other ++ "`."

        ParseError parseError ->
            case parseError of
                ModuleNameDoesntMatchFilePath { moduleName, filePath } ->
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
                    "The variable `"
                        ++ fullVarName var
                        ++ "` is not visible from the module `"
                        ++ module_
                        ++ "`. Have you imported it? Does it exist?"

        TypeError typeError ->
            case typeError of
                TypeMismatch t1 t2 ->
                    let
                        -- share index between types
                        ( type1, state1 ) =
                            TypeToString.toString TypeToString.emptyState t1

                        ( type2, _ ) =
                            TypeToString.toString state1 t2
                    in
                    "The types `"
                        ++ type1
                        ++ "` and `"
                        ++ type2
                        ++ "` don't match."

                OccursCheckFailed varId type_ ->
                    let
                        -- share index between types
                        ( type1, state1 ) =
                            TypeToString.toString TypeToString.emptyState (Type.Var varId)

                        ( type2, _ ) =
                            TypeToString.toString state1 type_
                    in
                    "An \"occurs check\" failed while typechecking: "
                        ++ type1
                        ++ " occurs in "
                        ++ type2

        EmitError emitError ->
            case emitError of
                MainDeclarationNotFound ->
                    "Couldn't find the value `main` in the main module given to the compiler!"

                ModuleNotFoundForVar { module_, var } ->
                    "Couldn't find the module `"
                        ++ module_
                        ++ "` when looking at the usage of variable `"
                        ++ var
                        ++ "`."

                ModuleNotFoundForType { module_, type_ } ->
                    "Couldn't find the module `"
                        ++ module_
                        ++ "` when looking at the usage of type `"
                        ++ type_
                        ++ "`."

                DeclarationNotFound { module_, name } ->
                    "Couldn't find the declaration `"
                        ++ name
                        ++ "` in the module `"
                        ++ module_
                        ++ "`."


fullVarName : { module_ : Maybe ModuleName, name : VarName } -> String
fullVarName { module_, name } =
    module_
        |> Maybe.map (\moduleAlias -> moduleAlias ++ "." ++ name)
        |> Maybe.withDefault name


parseErrorCode : { errorCode : String, filePath : FilePath } -> ErrorCode
parseErrorCode { errorCode, filePath } =
    case errorCode of
        "ENOENT" ->
            FileOrDirectoryNotFound filePath

        _ ->
            OtherErrorCode errorCode


type ErrorCode
    = FileOrDirectoryNotFound FilePath
    | OtherErrorCode String
