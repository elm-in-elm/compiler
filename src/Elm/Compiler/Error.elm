module Elm.Compiler.Error exposing
    ( Error(..), toString
    , ParseError(..), ParseProblem(..), ParseContext(..)
    , DesugarError(..)
    , TypeError(..)
    , EmitError(..)
    )

{-| All the errors the compiler can encounter.

@docs Error, toString
@docs ParseError, ParseProblem, ParseContext
@docs DesugarError
@docs TypeError
@docs EmitError

-}

import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type as Type exposing (Type)
import Elm.Data.Type.ToString as TypeToString
import Elm.Data.VarName exposing (VarName)
import Json.Decode as JD
import Parser.Advanced as P


{-| The top-level error type that breaks down into specific error types.
-}
type Error
    = ParseError ParseError
    | DesugarError DesugarError
    | TypeError TypeError
    | EmitError EmitError


{-| Errors encountered during [parsing](Elm.Compiler#parseExpr) from String to [AST](Elm.AST.Frontend).

The `DeadEnd` type in this definition is [the one from `elm/parser`](/packages/elm/parser/latest/Parser-Advanced#DeadEnd).

-}
type ParseError
    = ModuleNameDoesntMatchFilePath
        { moduleName : ModuleName
        , filePath : FilePath
        }
    | EmptySourceDirectories
    | InvalidElmJson JD.Error
    | ParseProblem (List (P.DeadEnd ParseContext ParseProblem))


{-| Context information about what was the parser trying to do at the time of
the error. Was it trying to parse an `if` expression? A list? etc.
-}
type ParseContext
    = InNumber
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


{-| The specific problem the parser encountered. Together with [`ParseContext`](#ParseContext)
and the [location info](Elm.Data.Located) this should give you enough info about what's wrong.
-}
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


{-| Errors encountered during [desugaring](Elm.Compiler#desugarExpr) from the [Frontend AST](Elm.AST.Frontend) to [Canonical AST](Elm.AST.Canonical).
-}
type DesugarError
    = VarNotInEnvOfModule
        { var : { module_ : Maybe ModuleName, name : VarName }
        , module_ : ModuleName
        }


{-| Errors encountered during [typechecking](Elm.Compiler#inferExpr).
-}
type TypeError
    = TypeMismatch Type Type
    | OccursCheckFailed Int Type


{-| Errors encountered during emitting. As you're free to do the emit phase however
you want, this is only returned from the helpers in Stage.Emit in the compiler CLI.

  - **TODO:** maybe expose Stage.Emit in this library (probably under the name
    `Elm.Compiler.Emit` or something similar)

-}
type EmitError
    = MainDeclarationNotFound
    | ModuleNotFoundForVar { module_ : ModuleName, var : VarName }
    | ModuleNotFoundForType { module_ : ModuleName, type_ : VarName }
    | DeclarationNotFound { module_ : ModuleName, name : VarName }


{-| An English description of the error. Feel free to write your own though!
-}
toString : Error -> String
toString error =
    case error of
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
                            :: List.map (\{ problem } -> "  " ++ parseProblemToString problem) problems
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


parseProblemToString : ParseProblem -> String
parseProblemToString problem =
    case problem of
        ExpectingPortKeyword ->
            "ExpectingPortKeyword"

        ExpectingEffectKeyword ->
            "ExpectingEffectKeyword"

        ExpectingModuleKeyword ->
            "ExpectingModuleKeyword"

        ExpectingModuleName ->
            "ExpectingModuleName"

        ExpectingExposingKeyword ->
            "ExpectingExposingKeyword"

        ExpectingExposingAllSymbol ->
            "ExpectingExposingAllSymbol"

        ExpectingExposingListLeftParen ->
            "ExpectingExposingListLeftParen"

        ExpectingExposingListRightParen ->
            "ExpectingExposingListRightParen"

        ExpectingExposingListSeparatorComma ->
            "ExpectingExposingListSeparatorComma"

        ExpectingExposedTypeDoublePeriod ->
            "ExpectingExposedTypeDoublePeriod"

        ExpectingVarName ->
            "ExpectingVarName"

        ExpectingTypeOrConstructorName ->
            "ExpectingTypeOrConstructorName"

        ExposingListCantBeEmpty ->
            "ExposingListCantBeEmpty"

        ExpectingImportKeyword ->
            "ExpectingImportKeyword"

        ExpectingAsKeyword ->
            "ExpectingAsKeyword"

        ExpectingModuleNameWithoutDots ->
            "ExpectingModuleNameWithoutDots"

        ExpectingModuleNamePart ->
            "ExpectingModuleNamePart"

        ExpectingQualifiedVarNameDot ->
            "ExpectingQualifiedVarNameDot"

        ExpectingEqualsSign ->
            "ExpectingEqualsSign"

        ExpectingMinusSign ->
            "ExpectingMinusSign"

        ExpectingNumber ->
            "ExpectingNumber"

        ExpectingSingleQuote ->
            "ExpectingSingleQuote"

        ExpectingChar ->
            "ExpectingChar"

        ExpectingEscapeBackslash ->
            "ExpectingEscapeBackslash"

        ExpectingEscapeCharacter char ->
            "ExpectingEscapeCharacter " ++ String.fromChar char

        ExpectingUnicodeEscapeLeftBrace ->
            "ExpectingUnicodeEscapeLeftBrace"

        ExpectingUnicodeEscapeRightBrace ->
            "ExpectingUnicodeEscapeRightBrace"

        InvalidUnicodeCodePoint ->
            "InvalidUnicodeCodePoint"

        ExpectingDoubleQuote ->
            "ExpectingDoubleQuote"

        ExpectingTripleQuote ->
            "ExpectingTripleQuote"

        ExpectingPlusOperator ->
            "ExpectingPlusOperator"

        ExpectingConsOperator ->
            "ExpectingConsOperator"

        ExpectingConcatOperator ->
            "ExpectingConcatOperator"

        ExpectingModuleDot ->
            "ExpectingModuleDot"

        ExpectingBackslash ->
            "ExpectingBackslash"

        ExpectingRightArrow ->
            "ExpectingRightArrow"

        ExpectingLeftParen ->
            "ExpectingLeftParen"

        ExpectingRightParen ->
            "ExpectingRightParen"

        ExpectingLeftBracket ->
            "ExpectingLeftBracket"

        ExpectingRightBracket ->
            "ExpectingRightBracket"

        ExpectingListSeparator ->
            "ExpectingListSeparator"

        ExpectingTupleSeparator ->
            "ExpectingTupleSeparator"

        ExpectingNotBeginningOfLine ->
            "ExpectingNotBeginningOfLine"

        ExpectingIf ->
            "ExpectingIf"

        ExpectingThen ->
            "ExpectingThen"

        ExpectingElse ->
            "ExpectingElse"

        ExpectingTrue ->
            "ExpectingTrue"

        ExpectingFalse ->
            "ExpectingFalse"

        ExpectingLet ->
            "ExpectingLet"

        ExpectingIn ->
            "ExpectingIn"

        ExpectingUnit ->
            "ExpectingUnit"

        InvalidNumber ->
            "InvalidNumber"

        TriedToParseCharacterStoppingDelimiter ->
            "TriedToParseCharacterStoppingDelimiter"

        CompilerBug bug ->
            "CompilerBug " ++ bug
