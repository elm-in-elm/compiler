module Elm.Compiler.Error exposing
    ( Error(..), toString
    , TokenizeError(..)
    , ParseError(..), ParseCompilerBug(..), ParseProblem(..), ParseContext(..)
    , DesugarError(..)
    , TypeError(..)
    , EmitError(..)
    )

{-| All the errors the compiler can encounter.

@docs Error, toString
@docs TokenizeError
@docs ParseError, ParseCompilerBug, ParseProblem, ParseContext
@docs DesugarError
@docs TypeError
@docs EmitError

-}

import Array
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Located exposing (Located)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..), Qualified)
import Elm.Data.Type exposing (TypeOrId(..))
import Elm.Data.Type.ToString as TypeToString
import Elm.Data.VarName exposing (VarName)
import Json.Decode as JD
import OurExtras.String as String
import Parser.Advanced as P


{-| The top-level error type that breaks down into specific error types.
-}
type Error
    = TokenizeError TokenizeError
    | ParseError ParseError
    | DesugarError DesugarError
    | TypeError TypeError
    | EmitError EmitError


{-| Errors encountered during [tokenizing](Elm.Compiler#tokenize) from `String` to `List Token`.
-}
type TokenizeError
    = EndOfCharNotFound
        { startLine : Int
        , startColumn : Int
        }
    | EndOfEscapeNotFound
        { startLine : Int
        , startColumn : Int
        }
    | EndOfUnicodeEscapeNotFound
        { startLine : Int
        , startColumn : Int
        }
    | EndOfDocCommentNotFound
        { startLine : Int
        , startColumn : Int
        }
    | FoundTabulator
        { line : Int
        , column : Int
        }
    | CharWasEmpty
        { startLine : Int
        , startColumn : Int
        }
    | CharTooLong
        { charContents : String
        , startLine : Int
        , startColumn : Int
        }
    | UnexpectedEscapeChar
        { char : Char
        , line : Int
        , column : Int
        }
    | WrongUnicodeEscapeLength
        { hexString : String
        , startLine : Int
        , startColumn : Int
        }
    | WrongUnicodeEscape
        { hexString : String
        , startLine : Int
        , startColumn : Int
        }
    | UnexpectedChar
        { char : Char
        , line : Int
        , column : Int
        }


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
    | ParseProblem ( List (P.DeadEnd ParseContext ParseProblem), FileContents )


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
    | InThreeDoubleQuotesString
    | InExpr
    | InIf
    | InLet
    | InLetBinding
    | InRecordBinding
    | InLambda
    | InList
    | InUnit
    | InTuple
    | InTuple3
    | InRecord
    | InFile FilePath
    | InCase
    | InPattern
    | InType
    | InTypeAlias
    | InCustomType
    | InConstructors
    | InTypeVarType
    | InUserDefinedType
    | InModuleNameWithDot
    | InQualifiers
    | InQualifiersAndUppercaseName
    | InParenthesizedType
    | InExposedValue
    | InDeclaration
    | InVar
    | InVarName
    | InNonqualifiedVar
    | InNonqualifiedConstructor
    | InQualifiedVar
    | InQualifiedConstructor
    | InTypeBinding
    | InPatternVar
    | InPatternRecord
    | InValueDeclaration
    | InPortDeclaration


{-| The specific problem the parser encountered. Together with [`ParseContext`](#ParseContext)
and the [location info](Elm.Data.Located) this should give you enough info about what's wrong.
-}
type ParseProblem
    = ExpectingPortKeyword -- `>port< module ...`
    | ExpectingEffectKeyword -- `>effect< module ...`
    | ExpectingModuleKeyword -- `>module< Foo.Bar exposing (..)`
    | ExpectingModuleName -- `module >Foo.Bar< exposing (..)`
    | ExpectingDot -- `foo : Bar>.<Baz`
    | ExpectingExposingKeyword -- `module Foo.Bar >exposing< (..)`
    | ExpectingExposingAllSymbol -- `module Foo.Bar exposing >(..)<`
    | ExpectingComma -- `module Foo.Bar exposing (a>,< b, c)`
    | ExpectingExposedTypeDoublePeriod -- `module Foo.Bar exposing (Foo>(..)<)`
    | ExpectingVarName -- eg. `module Foo.Bar exposing (>a<)`
    | ExpectingTypeOrConstructorName -- eg. `module Foo.Bar exposing (>Foo<)`
    | ExpectingTypeAlias -- eg. `>type alias <X =`
    | ExposingListCantBeEmpty -- `module Foo.Bar exposing >()<`
    | ExpectingImportKeyword -- `>import< Foo as F exposing (..)`
    | ExpectingAsKeyword -- `import Foo >as< F exposing (..)`
    | ExpectingUppercaseNameWithoutDots -- `import Foo as >F< exposing (..)`
    | ExpectingUppercaseNamePart -- `Foo.>Bar<.Baz.value`
    | ExpectingLowercaseNamePart
    | ExpectingEqualsSign -- `x >=< 1`
    | ExpectingMinusSign -- `>-<42`
    | ExpectingNumber
    | ExpectingApostrophe
    | ExpectingChar
    | ExpectingStringBoundary
    | ExpectingEscapeCharacter Char
    | ExpectingLeftBrace
    | ExpectingRightBrace
    | InvalidUnicodeCodePoint
    | ExpectingDoubleQuote
    | ExpectingTripleQuote
    | ExpectingPlusOperator
    | ExpectingConsOperator
    | ExpectingConcatOperator
    | ExpectingBackslash -- `>\<x -> x + 1`
    | ExpectingRightArrow -- `\x >->< x + 1`
    | ExpectingLeftParen
    | ExpectingRightParen
    | ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingNotBeginningOfLine
    | ExpectingIf
    | ExpectingThen
    | ExpectingElse
    | ExpectingTrue
    | ExpectingFalse
    | ExpectingLet
    | ExpectingIn
    | ExpectingUnit
    | ExpectingColon
    | ExpectingSpace
    | ExpectingPipe -- `Foo >|< Bar`
    | ExpectingSimpleType String
    | ExpectingListType
    | ExpectingCase
    | ExpectingOf
    | ExpectingCaseBody
    | ExpectingPatternAliasName -- `{ foo } as >bar<`
    | ExpectingIndentation
    | ExpectingNoIndentation
    | ExpectingPatternAnything -- `>_< ->`
    | ExpectingMaxThreeTuple
    | ExpectingUppercaseName
    | ExpectingNewlineAfterTypeAnnotation
    | ExpectingNonSpaceAfterTypeAnnotationNewlines
    | ExpectingZero
    | ExpectingLowercaseX
    | InvalidTab
    | InvalidNumber
    | MoreThanOneCharInApostrophes
    | StringContainedBadCharacters
    | ParseCompilerBug ParseCompilerBug
    | EmptyListOfConstructors
    | ExpectingEnd
    | IntCannotStartWithZero
    | FloatCannotEndWithDecimal
    | ExpectingScientificNotationE
    | ExpectingScientificNotationExponent
    | ExpectingScientificNotationPlus
    | ExpectingScientificNotationMinus
    | IntZeroCannotHaveScientificNotation


type ParseCompilerBug
    = ModuleNameStartParserFailed
    | ModuleNameEndParserFailed
    | QualifiersStartParserFailed
    | QualifiersSeparatorParserFailed
    | QualifiersEndParserFailed
    | ConstructorsStartParserFailed
    | ConstructorsSeparatorParserFailed
    | ConstructorsEndParserFailed
    | ParsedHexButCouldntConvert
    | ParsedIntButCouldntConvert


{-| Errors encountered during [desugaring](Elm.Compiler#desugarExpr) from the [Frontend AST](Elm.AST.Frontend) to [Canonical AST](Elm.AST.Canonical).
-}
type DesugarError
    = VarNameNotFound
        { var :
            { qualifiedness : PossiblyQualified
            , name : VarName
            }
        , insideModule : ModuleName
        }
    | AmbiguousName
        { name : VarName
        , insideModule : ModuleName
        , possibleModules : List ModuleName
        }
    | VarNameAndTypeAnnotationDontMatch
        { typeAnnotation : VarName
        , varName : VarName
        }
    | DuplicateRecordField
        { name : VarName
        , insideModule : ModuleName
        , firstOccurrence : Located ()
        , secondOccurrence : Located ()
        }


{-| Errors encountered during [typechecking](Elm.Compiler#inferExpr).
-}
type TypeError
    = TypeMismatch (TypeOrId Qualified) (TypeOrId Qualified)
    | OccursCheckFailed Int (TypeOrId Qualified)


{-| Errors encountered during emitting. As you're free to do the emit phase however
you want, this is only returned from the helpers in Stage.Emit in the compiler CLI.

  - **TODO:** maybe expose Stage.Emit in this library (probably under the name
    `Elm.Compiler.Emit` or something similar)

-}
type EmitError
    = MainDeclarationNotFound
    | ModuleNotFoundForVar { module_ : ModuleName, name : VarName }
    | DeclarationNotFound { module_ : ModuleName, name : VarName }
    | EmitCompilerBug String


{-| An English description of the error. Feel free to write your own though!
-}
toString : Error -> String
toString error =
    case error of
        TokenizeError tokenizeError ->
            case tokenizeError of
                EndOfCharNotFound r ->
                    "End of char not found; started at {LINE}:{COL}"
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                EndOfEscapeNotFound r ->
                    "End of escape not found; started at {LINE}:{COL}"
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                EndOfUnicodeEscapeNotFound r ->
                    "End of Unicode escape not found; started at {LINE}:{COL}"
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                EndOfDocCommentNotFound r ->
                    "End of doc comment not found; started at {LINE}:{COL}"
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                FoundTabulator r ->
                    "Found tabulator at {LINE}:{COL}"
                        |> String.replace "{LINE}" (String.fromInt r.line)
                        |> String.replace "{COL}" (String.fromInt r.column)

                CharWasEmpty r ->
                    "Character was empty; started at {LINE}:{COL}"
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                CharTooLong r ->
                    "Character too long: '{CHAR}'; started at {LINE}:{COL}"
                        |> String.replace "{CHAR}" r.charContents
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                UnexpectedEscapeChar r ->
                    "Unexpected escape char {CHAR} at {LINE}:{COL}"
                        |> String.replace "{CHAR}" (String.fromChar r.char)
                        |> String.replace "{LINE}" (String.fromInt r.line)
                        |> String.replace "{COL}" (String.fromInt r.column)

                WrongUnicodeEscapeLength r ->
                    "Wrong Unicode escape length for '\\u{{HEXSTRING}}' at {LINE}:{COL}"
                        |> String.replace "{HEXSTRING}" r.hexString
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                WrongUnicodeEscape r ->
                    "Wrong Unicode escape for '\\u{{HEXSTRING}}' at {LINE}:{COL}"
                        |> String.replace "{HEXSTRING}" r.hexString
                        |> String.replace "{LINE}" (String.fromInt r.startLine)
                        |> String.replace "{COL}" (String.fromInt r.startColumn)

                UnexpectedChar r ->
                    "Unexpected char {CHAR} at {LINE}:{COL}"
                        |> String.replace "{CHAR}" (String.fromChar r.char)
                        |> String.replace "{LINE}" (String.fromInt r.line)
                        |> String.replace "{COL}" (String.fromInt r.column)

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

                ParseProblem ( problems, source ) ->
                    String.join
                        "\n"
                        -- TODO combine multiple errors together rather than showing the source for all of them
                        (List.map
                            (\{ problem, row, col, contextStack } ->
                                multilineErrorMessage
                                    source
                                    contextStack
                                    (filenameFromContext contextStack)
                                    ("Parse problem: " ++ parseProblemToString problem)
                                    (parseProblemToString problem)
                                    { row = row, col = col }
                            )
                            problems
                        )

        DesugarError desugarError ->
            case desugarError of
                VarNameNotFound { var, insideModule } ->
                    "The variable `"
                        ++ fullVarName var
                        ++ "` is not visible from the module `"
                        ++ insideModule
                        ++ "`. Have you imported it? Does it exist?"

                AmbiguousName { name, insideModule, possibleModules } ->
                    "It's not clear which `"
                        ++ name
                        ++ "` you want, as there are multiple visible from within module `"
                        ++ insideModule
                        ++ "`:\n\n"
                        ++ String.join "\n"
                            (List.map
                                (\possibleModule ->
                                    " - "
                                        ++ fullVarName
                                            { qualifiedness = PossiblyQualified <| Just possibleModule
                                            , name = name
                                            }
                                )
                                possibleModules
                            )
                        ++ "\n\nChange your imports to resolve this ambiguity!"

                VarNameAndTypeAnnotationDontMatch { typeAnnotation, varName } ->
                    "The annotation and the definition below it don't match!\n\n"
                        ++ "  Annotation: "
                        ++ typeAnnotation
                        ++ "\n"
                        ++ "  Definition: "
                        ++ varName

                DuplicateRecordField { name } ->
                    {- TODO
                       This record has multiple `a` fields. One here:

                       12|     { a = 123
                                 ^
                       And another one here:

                       13|     , a = ()
                                 ^
                       How can I know which one you want? Rename one of them!
                    -}
                    "This record has multiple `" ++ name ++ "` fields."

        TypeError typeError ->
            case typeError of
                TypeMismatch t1 t2 ->
                    let
                        -- share index between types
                        ( type1, state1 ) =
                            TypeToString.toString
                                (TypeToString.fromTypesOrIds [ t1, t2 ])
                                t1

                        ( type2, _ ) =
                            TypeToString.toString state1 t2
                    in
                    "The types `"
                        ++ type1
                        ++ "` and `"
                        ++ type2
                        ++ "` don't match."

                OccursCheckFailed varId typeOrId ->
                    let
                        -- share index between types
                        ( type1, state1 ) =
                            TypeToString.toString
                                (TypeToString.fromTypeOrId typeOrId)
                                (Id varId)

                        ( type2, _ ) =
                            TypeToString.toString state1 typeOrId
                    in
                    "An \"occurs check\" failed while typechecking: "
                        ++ type1
                        ++ " occurs in "
                        ++ type2

        EmitError emitError ->
            case emitError of
                MainDeclarationNotFound ->
                    "Couldn't find the value `main` in the main module given to the compiler!"

                ModuleNotFoundForVar { module_, name } ->
                    "Couldn't find the module `"
                        ++ module_
                        ++ "` when looking at the usage of variable `"
                        ++ name
                        ++ "`."

                DeclarationNotFound { module_, name } ->
                    "Couldn't find the declaration `"
                        ++ name
                        ++ "` in the module `"
                        ++ module_
                        ++ "`."

                EmitCompilerBug bug ->
                    "Emit stage bug: " ++ bug


fullVarName : { qualifiedness : PossiblyQualified, name : VarName } -> String
fullVarName { qualifiedness, name } =
    let
        (PossiblyQualified maybeModule) =
            qualifiedness
    in
    maybeModule
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

        ExpectingComma ->
            "ExpectingComma"

        ExpectingExposedTypeDoublePeriod ->
            "ExpectingExposedTypeDoublePeriod"

        ExpectingVarName ->
            "ExpectingVarName"

        ExpectingTypeOrConstructorName ->
            "ExpectingTypeOrConstructorName"

        ExpectingTypeAlias ->
            "ExpectingTypeAlias"

        ExposingListCantBeEmpty ->
            "ExposingListCantBeEmpty"

        ExpectingImportKeyword ->
            "ExpectingImportKeyword"

        ExpectingAsKeyword ->
            "ExpectingAsKeyword"

        ExpectingUppercaseNameWithoutDots ->
            "ExpectingUppercaseNameWithoutDots"

        ExpectingUppercaseNamePart ->
            "ExpectingUppercaseNamePart"

        ExpectingLowercaseNamePart ->
            "ExpectingLowercaseNamePart"

        ExpectingDot ->
            "ExpectingDot"

        ExpectingEqualsSign ->
            "ExpectingEqualsSign"

        ExpectingMinusSign ->
            "ExpectingMinusSign"

        ExpectingNumber ->
            "ExpectingNumber"

        ExpectingApostrophe ->
            "ExpectingApostrophe"

        ExpectingChar ->
            "ExpectingChar"

        ExpectingStringBoundary ->
            "ExpectingStringBoundary"

        ExpectingEscapeCharacter char ->
            "ExpectingEscapeCharacter " ++ String.fromChar char

        ExpectingLeftBrace ->
            "ExpectingLeftBrace"

        ExpectingRightBrace ->
            "ExpectingRightBrace"

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

        ExpectingColon ->
            "ExpectingColon"

        ExpectingSpace ->
            "ExpectingSpace"

        ExpectingPipe ->
            "ExpectingPipe"

        ExpectingSimpleType type_ ->
            "ExpectingSimpleType " ++ type_

        ExpectingListType ->
            "ExpectingListType"

        ExpectingCase ->
            "ExpectingCase"

        ExpectingOf ->
            "ExpectingOf"

        ExpectingCaseBody ->
            "ExpectingCaseBody"

        ExpectingPatternAliasName ->
            "ExpectingPatternAliasName"

        ExpectingIndentation ->
            "ExpectingIndentation"

        ExpectingNoIndentation ->
            "ExpectingNoIndentation"

        ExpectingPatternAnything ->
            "ExpectingPatternAnything"

        ExpectingMaxThreeTuple ->
            "ExpectingMaxThreeTuple"

        ExpectingUppercaseName ->
            "ExpectingUppercaseName"

        ExpectingNewlineAfterTypeAnnotation ->
            "ExpectingNewlineAfterTypeAnnotation"

        ExpectingNonSpaceAfterTypeAnnotationNewlines ->
            "ExpectingNonSpaceAfterTypeAnnotationNewlines"

        ExpectingZero ->
            "ExpectingZero"

        ExpectingLowercaseX ->
            "ExpectingLowercaseX"

        InvalidTab ->
            "InvalidTab"

        InvalidNumber ->
            "InvalidNumber"

        MoreThanOneCharInApostrophes ->
            "MoreThanOneCharInApostrophes"

        StringContainedBadCharacters ->
            "StringContainedBadCharacters"

        ParseCompilerBug bug ->
            "Parse compiler bug: "
                ++ parseCompilerBugToString bug

        EmptyListOfConstructors ->
            "EmptyListOfConstructors"

        ExpectingEnd ->
            "ExpectingEnd"

        IntCannotStartWithZero ->
            "IntCannotStartWithZero"

        FloatCannotEndWithDecimal ->
            "FloatCannotEndWithDecimal"

        ExpectingScientificNotationE ->
            "ExpectingScientificNotationE"

        ExpectingScientificNotationExponent ->
            "ExpectingScientificNotationExponent"

        ExpectingScientificNotationPlus ->
            "ExpectingScientificNotationPlus"

        ExpectingScientificNotationMinus ->
            "ExpectingScientificNotationMinus"

        IntZeroCannotHaveScientificNotation ->
            "IntZeroCannotHaveScientificNotation"


parseCompilerBugToString : ParseCompilerBug -> String
parseCompilerBugToString bug =
    case bug of
        ModuleNameStartParserFailed ->
            "moduleName start parser failed"

        ModuleNameEndParserFailed ->
            "moduleName end parser failed"

        QualifiersStartParserFailed ->
            "qualifiers start parser failed"

        QualifiersSeparatorParserFailed ->
            "qualifiers separator parser failed"

        QualifiersEndParserFailed ->
            "qualifiers end parser failed"

        ConstructorsStartParserFailed ->
            "constructors start parser failed"

        ConstructorsSeparatorParserFailed ->
            "constructors separator parser failed"

        ConstructorsEndParserFailed ->
            "constructors end parser failed"

        ParsedHexButCouldntConvert ->
            "parsed hex characters but couldn't convert them to an integer"

        ParsedIntButCouldntConvert ->
            "parsed int characters but couldn't convert them to an integer"


filenameFromContext : List { a | context : ParseContext } -> Maybe FilePath
filenameFromContext contextStack_ =
    case contextStack_ of
        { context } :: rest ->
            case context of
                InFile name ->
                    Just name

                _ ->
                    filenameFromContext rest

        [] ->
            Nothing


multilineErrorMessage : FileContents -> List { a | context : ParseContext } -> Maybe FilePath -> String -> String -> { row : Int, col : Int } -> String
multilineErrorMessage source contextStack filename title errorMessage { row, col } =
    title
        ++ "\n  --> "
        ++ (filename
                |> Maybe.map (\s -> s ++ ":")
                |> Maybe.withDefault ""
           )
        ++ String.fromInt row
        ++ ":"
        ++ String.fromInt col
        ++ (source
                |> String.split "\n"
                |> Array.fromList
                |> Array.get (row - 1)
                |> Maybe.map
                    (\snippet ->
                        "\n   | "
                            ++ "\n"
                            ++ String.padRight 3 ' ' (String.fromInt row)
                            ++ "| "
                            ++ snippet
                            ++ "\n   | "
                            ++ String.repeat (col - 1) " "
                            ++ "^ "
                            ++ errorMessage
                    )
                |> Maybe.withDefault ""
           )
        ++ "\n"
        ++ contextStackToString contextStack
        ++ "\n"


contextStackToString : List { a | context : ParseContext } -> String
contextStackToString contextStack =
    "Context stack:\n"
        ++ (contextStack
                |> List.map (.context >> parseContextToString >> String.indent)
                |> String.join "\n"
           )


parseContextToString : ParseContext -> String
parseContextToString context =
    case context of
        InNumber ->
            "InNumber"

        InChar ->
            "InChar"

        InCharEscapeMode ->
            "InCharEscapeMode"

        InUnicodeCharacter ->
            "InUnicodeCharacter"

        InString ->
            "InString"

        InDoubleQuoteString ->
            "InDoubleQuoteString"

        InThreeDoubleQuotesString ->
            "InThreeDoubleQuotesString"

        InExpr ->
            "InExpr"

        InIf ->
            "InIf"

        InLet ->
            "InLet"

        InLetBinding ->
            "InLetBinding"

        InRecordBinding ->
            "InRecordBinding"

        InLambda ->
            "InLambda"

        InList ->
            "InList"

        InUnit ->
            "InUnit"

        InTuple ->
            "InTuple"

        InTuple3 ->
            "InTuple3"

        InRecord ->
            "InRecord"

        InFile path ->
            "InFile " ++ path

        InCase ->
            "InCase"

        InPattern ->
            "InPattern"

        InType ->
            "InType"

        InTypeAlias ->
            "InTypeAlias"

        InCustomType ->
            "InCustomType"

        InConstructors ->
            "InConstructors"

        InTypeVarType ->
            "InTypeVarType"

        InUserDefinedType ->
            "InUserDefinedType"

        InModuleNameWithDot ->
            "InModuleNameWithDot"

        InQualifiers ->
            "InQualifiers"

        InQualifiersAndUppercaseName ->
            "InQualifiersAndUppercaseName"

        InParenthesizedType ->
            "InParenthesizedType"

        InExposedValue ->
            "InExposedValue"

        InDeclaration ->
            "InDeclaration"

        InVar ->
            "InVar"

        InVarName ->
            "InVarName"

        InNonqualifiedVar ->
            "InNonqualifiedVar"

        InNonqualifiedConstructor ->
            "InNonqualifiedConstructor"

        InQualifiedVar ->
            "InQualifiedVar"

        InQualifiedConstructor ->
            "InQualifiedConstructor"

        InTypeBinding ->
            "InTypeBinding"

        InPatternVar ->
            "InPatternVar"

        InPatternRecord ->
            "InPatternRecord"

        InValueDeclaration ->
            "InValueDeclaration"

        InPortDeclaration ->
            "InPortDeclaration"
