module Elm.Compiler.Error exposing
    ( Error(..), toString
    , TokenizeError(..)
    , ParseError(..), LocatedParseError, LocatedParseErrorType(..)
    , DesugarError(..)
    , TypeError(..)
    , EmitError(..)
    )

{-| All the errors the compiler can encounter.

@docs Error, toString
@docs TokenizeError
@docs ParseError, LocatedParseError, LocatedParseErrorType
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
import Elm.Data.Token as Token exposing (Token)
import Elm.Data.Type exposing (TypeOrId(..))
import Elm.Data.Type.ToString as TypeToString
import Elm.Data.VarName exposing (VarName)
import Json.Decode as JD
import OurExtras.String as String


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
    | EndOfStringNotFound
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
    | TokenizeCompilerBug String


{-| Errors encountered during [parsing](Elm.Compiler#parseExpr) from `List Token` to [AST](Elm.AST.Frontend).
-}
type ParseError
    = ModuleNameDoesntMatchFilePath
        { moduleName : ModuleName
        , filePath : FilePath
        }
      -- TODO EmptySourceDirectories?
    | InvalidElmJson JD.Error
    | LocatedError LocatedParseError


type alias LocatedParseError =
    { line : Int
    , column : Int
    , type_ : LocatedParseErrorType
    }


type LocatedParseErrorType
    = EmptyOneOf
    | ExpectedEOF (List Token)
    | ExpectedNonemptyList
    | ExpectedMaxThreeTuple
    | ExpectedModuleNameWithoutDots
    | ExpectedToken Token.Type
    | ExpectedTokenT Token.T
    | TokenDidNotContainString Token.T
    | TokenDidNotContainInt Token.T
    | TokenDidNotContainFloat Token.T
    | TokenDidNotContainChar Token.T
    | ParseCompilerBug String


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

                EndOfStringNotFound r ->
                    "End of string not found; started at {LINE}:{COL}"
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

                TokenizeCompilerBug bug ->
                    "Tokenize compiler bug: " ++ bug

        ParseError parseError ->
            case parseError of
                ModuleNameDoesntMatchFilePath { moduleName, filePath } ->
                    "Module name `"
                        ++ moduleName
                        ++ "` doesn't match the file path `"
                        ++ filePath
                        ++ "`."

                InvalidElmJson jsonError ->
                    "Invalid elm.json! "
                        ++ JD.errorToString jsonError

                LocatedError err ->
                    """
                    Parse error at {LINE}:{COL}
                    ---------------------------
                    {ERROR}
                    """
                        |> String.multilineInput
                        |> String.replace "{LINE}" (String.fromInt err.line)
                        |> String.replace "{COL}" (String.fromInt err.column)
                        |> String.replace "{ERROR}" (locatedParseErrorTypeToString err.type_)

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


locatedParseErrorTypeToString : LocatedParseErrorType -> String
locatedParseErrorTypeToString type_ =
    case type_ of
        EmptyOneOf ->
            "Empty oneOf"

        ExpectedEOF tokens ->
            "Expected EOF, got "
                ++ String.fromInt (List.length tokens)
                ++ " tokens, starting with:\n"
                ++ (tokens
                        |> List.take 5
                        |> List.map (Token.toString >> (\s -> " - " ++ s))
                        |> String.join "\n"
                   )

        ExpectedNonemptyList ->
            "Expected non-empty list"

        ExpectedMaxThreeTuple ->
            "Expected tuple of max length 3"

        ExpectedModuleNameWithoutDots ->
            "Expected module name without dots"

        ExpectedToken t ->
            "Expected token of type " ++ Token.typeToString t

        ExpectedTokenT t ->
            "Expected token of type " ++ Token.tToString t

        TokenDidNotContainString t ->
            "Token did not contain string: " ++ Token.tToString t

        TokenDidNotContainInt t ->
            "Token did not contain int: " ++ Token.tToString t

        TokenDidNotContainFloat t ->
            "Token did not contain float: " ++ Token.tToString t

        TokenDidNotContainChar t ->
            "Token did not contain char: " ++ Token.tToString t

        ParseCompilerBug string ->
            "Parse compiler bug: " ++ string


fullVarName : { qualifiedness : PossiblyQualified, name : VarName } -> String
fullVarName { qualifiedness, name } =
    let
        (PossiblyQualified maybeModule) =
            qualifiedness
    in
    maybeModule
        |> Maybe.map (\moduleAlias -> moduleAlias ++ "." ++ name)
        |> Maybe.withDefault name
