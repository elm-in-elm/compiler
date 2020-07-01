module Elm.Compiler.Error exposing
    ( Error(..), toString
    , ParseError(..), ParseCompilerBug(..), ParseProblem(..), ParseContext(..)
    , DesugarError(..)
    , TypeError(..)
    , EmitError(..)
    )

{-| All the errors the compiler can encounter.

@docs Error, toString
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
    | InRecord
    | InRecordBinding
    | InFile FilePath
    | InCase
    | InPattern
    | InTypeAnnotation
    | InType
    | InTypeAlias
    | InCustomType
    | InConstructors
    | InTypeVarType
    | InUserDefinedType
    | InModuleNameWithDot
    | InQualifiers
    | InQualifiersAndTypeName
    | InParenthesizedType
    | InExposedValue
    | InDeclaration
    | InVar
    | InVarName
    | InQualifiedVar
    | InTypeBinding
    | InPatternVar
    | InPatternRecord


{-| The specific problem the parser encountered. Together with [`ParseContext`](#ParseContext)
and the [location info](Elm.Data.Located) this should give you enough info about what's wrong.
-}
type ParseProblem
    = TooMuchIndentation String
    | ExpectingPortKeyword -- `>port< module ...`
    | ExpectingEffectKeyword -- `>effect< module ...`
    | ExpectingModuleKeyword -- `>module< Foo.Bar exposing (..)`
    | ExpectingModuleName -- `module >Foo.Bar< exposing (..)`
    | ExpectingModuleNameDot -- `foo : Bar>.<Baz`
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
    | ExpectingLeftBrace
    | ExpectingRightBrace
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
    | ExpectingLetIndentation
    | ExpectingLetBindingIndentation
    | ExpectingIn
    | ExpectingUnit
    | ExpectingColon
    | ExpectingSpace
    | ExpectingPipe -- `Foo >|< Bar`
    | ExpectingSimpleType String
    | ExpectingListType
    | ExpectingRecordLeftBrace
    | ExpectingRecordSeparator
    | ExpectingRecordRightBrace
    | ExpectingCase
    | ExpectingOf
    | ExpectingCaseBody
    | ExpectingPatternAliasName -- `{ foo } as >bar<`
    | ExpectingIndentation
    | ExpectingPatternAnything -- `>_< ->`
    | ExpectingMaxThreeTuple
    | ExpectingTypeName
    | ExpectingNewlineAfterTypeAnnotation
    | ExpectingNonSpaceAfterTypeAnnotationNewlines
    | InvalidTab
    | InvalidNumber
    | TriedToParseCharacterStoppingDelimiter
    | ParseCompilerBug ParseCompilerBug
    | EmptyListOfConstructors


type ParseCompilerBug
    = ModuleNameStartParserFailed
    | ModuleNameEndParserFailed
    | MultipleCharactersChompedInCharacter
    | QualifiersStartParserFailed
    | QualifiersSeparatorParserFailed
    | QualifiersEndParserFailed
    | ConstructorsStartParserFailed
    | ConstructorsSeparatorParserFailed
    | ConstructorsEndParserFailed


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
    | ModuleNotFoundForType { module_ : ModuleName, type_ : VarName }
    | DeclarationNotFound { module_ : ModuleName, name : VarName }
    | EmitCompilerBug String


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

                ParseProblem ( problems, source ) ->
                    String.join
                        "\n"
                        (List.map
                            (\{ problem, row, col, contextStack } ->
                                multilineErrorMessage
                                    source
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
        TooMuchIndentation str ->
            {- TODO
               Too Much Indentation
               Line 1, Column 2
               This `module` should not have any spaces before it:

               1|  module Main
                   ^
               Delete the spaces before `module` until there are none left!
            -}
            "TooMuchIndentation " ++ str

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

        ExpectingModuleNameWithoutDots ->
            "ExpectingModuleNameWithoutDots"

        ExpectingModuleNamePart ->
            "ExpectingModuleNamePart"

        ExpectingModuleNameDot ->
            "ExpectingModuleNameDot"

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

        ExpectingLetIndentation ->
            {- TODO
               Unfinished Let
               Line 20, Column 6
               I was partway through parsing a `let` expression, but I got stuck here:

               20|   let
                        ^
               I was expecting a value to be defined here.

               Note: Here is an example with a valid `let` expression for reference:

                   viewPerson person =
                     let
                       fullName =
                         person.firstName ++ " " ++ person.lastName
                     in
                     div [] [ text fullName ]

               Here we defined a `viewPerson` function that turns a person into some HTML. We
               use a `let` expression to define the `fullName` we want to show. Notice the
               indentation! The `fullName` is indented more than the `let` keyword, and the
               actual value of `fullName` is indented a bit more than that. That is important!
            -}
            "ExpectingLetIndentation"

        ExpectingLetBindingIndentation ->
            {- TODO
               ERRORS
               Unexpected Equals
               Line 22, Column 7
               I was not expecting to see this equals sign:

               22|     y = 2 in { count = 0 }
                         ^
               Maybe you want == instead? To check if two values are equal?

               Note: I may be getting confused by your indentation. I think I am still parsing
               the `x` definition. Is this supposed to be part of a definition after that? If
               so, the problem may be a bit before the equals sign. I need all definitions to
               be indented exactly the same amount, so the problem may be that this new
               definition has too many spaces in front of it.
            -}
            "ExpectingLetBindingIndentation"

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

        ExpectingRecordLeftBrace ->
            "ExpectingRecordLeftBrace"

        ExpectingRecordSeparator ->
            "ExpectingRecordSeparator"

        ExpectingRecordRightBrace ->
            "ExpectingRecordRightBrace"

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

        ExpectingPatternAnything ->
            "ExpectingPatternAnything"

        ExpectingMaxThreeTuple ->
            "ExpectingMaxThreeTuple"

        ExpectingTypeName ->
            "ExpectingTypeName"

        ExpectingNewlineAfterTypeAnnotation ->
            "ExpectingNewlineAfterTypeAnnotation"

        ExpectingNonSpaceAfterTypeAnnotationNewlines ->
            "ExpectingNonSpaceAfterTypeAnnotationNewlines"

        InvalidTab ->
            "InvalidTab"

        InvalidNumber ->
            "InvalidNumber"

        TriedToParseCharacterStoppingDelimiter ->
            "TriedToParseCharacterStoppingDelimiter"

        ParseCompilerBug bug ->
            "Parse compiler bug: "
                ++ parseCompilerBugToString bug

        EmptyListOfConstructors ->
            "EmptyListOfConstructors"


parseCompilerBugToString : ParseCompilerBug -> String
parseCompilerBugToString bug =
    case bug of
        ModuleNameStartParserFailed ->
            "moduleName start parser failed"

        ModuleNameEndParserFailed ->
            "moduleName end parser failed"

        MultipleCharactersChompedInCharacter ->
            "multiple characters chomped in character"

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


multilineErrorMessage : FileContents -> Maybe FilePath -> String -> String -> { row : Int, col : Int } -> String
multilineErrorMessage source filename title errorMessage { row, col } =
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
