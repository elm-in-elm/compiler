module Stage.Parse.Parser exposing
    ( customTypeDeclaration
    , declaration
    , exposingList
    , expr
    , import_
    , imports
    , moduleDeclaration
    , moduleName
    , module_
    , spacesAndComments
    , typeAliasDeclaration
    , type_
    , valueDeclaration
    )

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Compiler.Error
    exposing
        ( Error(..)
        , ParseCompilerBug(..)
        , ParseContext(..)
        , ParseError(..)
        , ParseProblem(..)
        )
import Elm.Data.Binding as Binding exposing (Binding)
import Elm.Data.Comment exposing (Comment, CommentType(..))
import Elm.Data.Declaration as Declaration
    exposing
        ( Constructor
        , Declaration
        , DeclarationBody
        )
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Import exposing (Import)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Hex
import List.NonEmpty exposing (NonEmpty)
import Parser.Advanced as P exposing ((|.), (|=), Parser)
import Pratt.Advanced as PP
import Set exposing (Set)


type alias Parser_ a =
    Parser ParseContext ParseProblem a


type alias ExprConfig =
    PP.Config ParseContext ParseProblem ( LocatedExpr, List Comment )


type alias PatternConfig =
    PP.Config ParseContext ParseProblem ( LocatedPattern, List Comment )


type alias TypeConfig =
    PP.Config ParseContext ParseProblem ( ConcreteType PossiblyQualified, List Comment )


located : Parser_ p -> Parser_ (Located p)
located p =
    P.succeed
        (\( startRow, startCol ) value ( endRow, endCol ) ->
            Located.located
                { start = { row = startRow, col = startCol }
                , end = { row = endRow, col = endCol }
                }
                value
        )
        |= P.getPosition
        |= p
        |= P.getPosition


module_ : FilePath -> Parser_ (Module LocatedExpr TypeAnnotation PossiblyQualified)
module_ filePath =
    P.succeed
        (\startComments ( moduleType_, moduleName_, exposing_ ) ( imports_, ( declarations_, commentsAfterDeclarations ) ) ->
            { imports = imports_
            , name = moduleName_
            , filePath = filePath
            , declarations =
                declarations_
                    |> List.map
                        (\almostDeclaration ->
                            let
                                declaration_ =
                                    almostDeclaration moduleName_
                            in
                            ( declaration_.name, declaration_ )
                        )
                    |> Dict.fromList
            , type_ = moduleType_
            , exposing_ = exposing_
            , startComments = startComments
            , endComments = commentsAfterDeclarations
            }
        )
        |= spacesAndComments
        |= moduleDeclaration
        |= (spacesAndComments
                |> P.andThen imports
                |> P.andThen
                    (\( imports_, commentsAfterImports ) ->
                        declarations commentsAfterImports
                            |> P.map (Tuple.pair imports_)
                    )
           )
        |> P.withIndent 1
        |> P.inContext (InFile filePath)


moduleDeclaration : Parser_ ( ModuleType, ModuleName, Exposing )
moduleDeclaration =
    P.succeed
        (\moduleType_ moduleName_ exposing_ ->
            ( moduleType_, moduleName_, exposing_ )
        )
        |= moduleType
        |. spacesCommentsAndGreaterIndent
        |= moduleName
        |. spacesCommentsAndGreaterIndent
        |. P.keyword (P.Token "exposing" ExpectingExposingKeyword)
        |. spacesCommentsAndGreaterIndent
        |= exposingList


imports : List Comment -> Parser_ ( Dict ModuleName Import, List Comment )
imports commentsBefore =
    zeroOrMoreWithSpacesAndCommentsInBetween import_ commentsBefore
        |> P.map
            (Tuple.mapFirst
                (List.map (\dep -> ( dep.moduleName, dep ))
                    >> Dict.fromList
                )
            )


import_ : Parser_ (List Comment -> ( Import, List Comment ))
import_ =
    P.succeed
        (\commentsBeforeModuleName moduleName_ commentsAfterModuleName as_ exposing_ commentsBefore ->
            let
                adjusts =
                    case ( as_, exposing_ ) of
                        ( Nothing, Nothing ) ->
                            -- Comments after the module name will be
                            -- passed to the next parser.
                            { commentsAfterModuleName = []
                            , as_ = Nothing
                            , exposing_ = Nothing
                            , commentsAfter = commentsAfterModuleName
                            }

                        ( Just as__, Nothing ) ->
                            -- Comments after the alias module name will
                            -- be passed to the next parser.
                            { commentsAfterModuleName = commentsAfterModuleName
                            , as_ = Just { as__ | commentsAfterAs = [] }
                            , exposing_ = Nothing
                            , commentsAfter = as__.commentsAfterAs
                            }

                        ( _, Just ( exposing__, commentsAfter ) ) ->
                            -- Comments after the exposing will be passed
                            -- to the next parser.
                            { commentsAfterModuleName = commentsAfterModuleName
                            , as_ = as_
                            , exposing_ = Just exposing__
                            , commentsAfter = commentsAfter
                            }
            in
            ( { commentsBefore = commentsBefore
              , commentsBeforeModuleName = commentsBeforeModuleName
              , moduleName = moduleName_
              , commentsAfterModuleName = adjusts.commentsAfterModuleName
              , as_ = adjusts.as_
              , exposing_ = adjusts.exposing_
              }
            , adjusts.commentsAfter
            )
        )
        |. P.keyword (P.Token "import" ExpectingImportKeyword)
        |. checkTooMuchIndentation "import"
        |= spacesCommentsAndGreaterIndent
        |= moduleName
        |= spacesAndComments
        |= P.oneOf
            [ P.succeed
                (\commentsBeforeAs as_ commentsAfterAs ->
                    Just
                        { commentsBeforeAs = commentsBeforeAs
                        , as_ = as_
                        , commentsAfterAs = commentsAfterAs
                        }
                )
                |. checkIndent (<) ExpectingIndentation
                |. P.keyword (P.Token "as" ExpectingAsKeyword)
                |= spacesCommentsAndGreaterIndent
                |= moduleNameWithoutDots
                |. P.oneOf
                    [ P.symbol (P.Token "." ExpectingModuleNameWithoutDots)
                        |> P.andThen (always (P.problem ExpectingModuleNameWithoutDots))
                    , P.succeed ()
                    ]
                |= spacesAndComments
            , P.succeed Nothing
            ]
        |= P.oneOf
            [ P.succeed
                (\commentsBeforeExposing exposing_ commentsAfterExposing ->
                    Just
                        ( { commentsBeforeExposing = commentsBeforeExposing
                          , exposing_ = exposing_
                          }
                        , commentsAfterExposing
                        )
                )
                |. checkIndent (<) ExpectingIndentation
                |. P.keyword (P.Token "exposing" ExpectingExposingKeyword)
                |= spacesCommentsAndGreaterIndent
                |= exposingList
                |= spacesAndComments
            , P.succeed Nothing
            ]
        |> P.inContext InImport


moduleType : Parser_ ModuleType
moduleType =
    P.oneOf
        [ plainModuleType
        , portModuleType
        , effectModuleType
        ]


plainModuleType : Parser_ ModuleType
plainModuleType =
    P.succeed PlainModule
        |. P.keyword (P.Token "module" ExpectingModuleKeyword)
        |. checkTooMuchIndentation "module"


portModuleType : Parser_ ModuleType
portModuleType =
    P.succeed PortModule
        |. P.keyword (P.Token "port" ExpectingPortKeyword)
        |. checkTooMuchIndentation "port"
        |. spacesCommentsAndGreaterIndent
        |. P.keyword (P.Token "module" ExpectingModuleKeyword)


effectModuleType : Parser_ ModuleType
effectModuleType =
    -- TODO some metadata?
    P.succeed EffectModule
        |. P.keyword (P.Token "effect" ExpectingEffectKeyword)
        |. checkTooMuchIndentation "effect"
        |. spacesCommentsAndGreaterIndent
        |. P.keyword (P.Token "module" ExpectingModuleKeyword)


moduleName : Parser_ String
moduleName =
    P.loop [] moduleNameHelp


moduleNameHelp : List String -> Parser_ (P.Step (List String) String)
moduleNameHelp acc =
    P.succeed
        (\moduleNameStr continue ->
            if continue then
                P.Loop (moduleNameStr :: acc)

            else
                List.reverse (moduleNameStr :: acc)
                    |> String.join "."
                    |> P.Done
        )
        |= moduleNameWithoutDots
        |= P.oneOf
            [ P.symbol (P.Token "." ExpectingModuleDot)
                |> P.map (\_ -> True)
            , P.succeed False
            ]


moduleNameWithoutDots : Parser_ String
moduleNameWithoutDots =
    P.variable
        { start = Char.isUpper
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        , expecting = ExpectingModuleNamePart
        }


exposingList : Parser_ Exposing
exposingList =
    P.oneOf
        [ exposingAll
        , exposingSome
        ]


exposingAll : Parser_ Exposing
exposingAll =
    P.symbol (P.Token "(..)" ExpectingExposingAllSymbol)
        |> P.map (always ExposingAll)


exposingSome : Parser_ Exposing
exposingSome =
    P.sequence
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingComma
        , end = P.Token ")" ExpectingRightParen
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore item commentsAfter ->
                    { commentsBefore = commentsBefore
                    , item = item
                    , commentsAfter = commentsAfter
                    }
                )
                |= spacesCommentsAndGreaterIndent
                |= exposedItem
                |= spacesCommentsAndGreaterIndent
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\list_ ->
                if List.isEmpty list_ then
                    P.problem ExposingListCantBeEmpty

                else
                    P.succeed (ExposingSome list_)
            )


exposedItem : Parser_ ExposedItem
exposedItem =
    P.oneOf
        [ exposedValue
        , exposedTypeAndOptionallyAllConstructors
        ]


exposedValue : Parser_ ExposedItem
exposedValue =
    P.map ExposedValue varName
        |> P.inContext InExposedValue


exposedTypeAndOptionallyAllConstructors : Parser_ ExposedItem
exposedTypeAndOptionallyAllConstructors =
    P.succeed
        (\name hasDoublePeriod ->
            if hasDoublePeriod then
                ExposedTypeAndAllConstructors name

            else
                ExposedType name
        )
        |= typeOrConstructorName
        |= P.oneOf
            [ P.succeed True
                |. P.symbol (P.Token "(..)" ExpectingExposedTypeDoublePeriod)
            , P.succeed False
            ]


typeOrConstructorName : Parser_ String
typeOrConstructorName =
    P.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        , expecting = ExpectingTypeOrConstructorName
        }


{-| Taken from the official compiler.
-}
reservedWords : Set String
reservedWords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


declarations : List Comment -> Parser_ ( List (ModuleName -> Declaration LocatedExpr TypeAnnotation PossiblyQualified), List Comment )
declarations commentsBefore =
    zeroOrMoreWithSpacesAndCommentsInBetween declaration commentsBefore


declaration : Parser_ (List Comment -> ( ModuleName -> Declaration LocatedExpr TypeAnnotation PossiblyQualified, List Comment ))
declaration =
    P.succeed
        (\( name, body, commentsAfter ) commentsBefore ->
            ( \module__ ->
                { module_ = module__
                , name = name
                , commentsBefore = commentsBefore
                , body = body
                }
            , commentsAfter
            )
        )
        |= declarationBody
        |> P.inContext InDeclaration


declarationBody : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified, List Comment )
declarationBody =
    P.oneOf
        [ typeAliasDeclaration
        , customTypeDeclaration
        , valueDeclaration Nothing
        ]


{-|

     type alias X = Int
     type alias X a = Maybe a

More generally,

     type alias <UserDefinedType> <VarType>* = <Type>

-}
typeAliasDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified, List Comment )
typeAliasDeclaration =
    P.succeed
        (\commentsBeforeName name ( parameters, commentsAfterParameters ) ( type__, commentsAfter ) ->
            ( name
            , Declaration.TypeAlias
                { parameters = parameters
                , definition = type__
                }
            , commentsAfter
            )
        )
        |. P.keyword (P.Token "type alias" ExpectingTypeAlias)
        |= spacesCommentsAndGreaterIndent
        |= moduleNameWithoutDots
        |= (spacesCommentsAndGreaterIndent
                |> P.andThen
                    (zeroOrMoreWithSpacesAndCommentsInBetween
                        (P.succeed
                            (\parameter commentsAfterParameter commentsBefore ->
                                {- TODO -}
                                ( parameter
                                , commentsAfterParameter
                                )
                            )
                            |= varName
                            |= spacesCommentsAndGreaterIndent
                        )
                    )
           )
        |. spacesCommentsAndGreaterIndent
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |. spacesCommentsAndGreaterIndent
        |= type_
        |> P.inContext InTypeAlias


{-|

     type X = Foo | Bar
     type X a = Foo a | Bar String

More generally,

     type <UserDefinedType> <VarType>* = <Constructor>[ | <Constructor>]*

     Constructor := <Name>[ <Type>]*

-}
customTypeDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified, List Comment )
customTypeDeclaration =
    P.succeed
        (\name ( parameters, commentsAfterParameters {- TODO -} ) ( constructors_, commentsAfter ) ->
            ( name
            , Declaration.CustomType
                { parameters = parameters
                , constructors = constructors_
                }
            , commentsAfter
            )
        )
        |. P.keyword (P.Token "type" ExpectingTypeAlias)
        |. spacesCommentsAndGreaterIndent
        |= moduleNameWithoutDots
        |= (spacesCommentsAndGreaterIndent
                |> P.andThen
                    (zeroOrMoreWithSpacesAndCommentsInBetween
                        (P.succeed
                            (\parameter commentsAfterParameter commentsBefore ->
                                {- TODO -}
                                ( parameter
                                , commentsAfterParameter
                                )
                            )
                            |= varName
                            |= spacesCommentsAndGreaterIndent
                        )
                    )
           )
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |= constructors
        |> P.inContext InCustomType


constructors : Parser_ ( NonEmpty (Constructor PossiblyQualified), List Comment )
constructors =
    {- TODO store comments -}
    P.succeed Tuple.pair
        |= spacesCommentsAndGreaterIndent
        |= P.oneOf
            [ constructor
            , P.succeed ()
                |> P.andThen (\_ -> P.problem EmptyListOfConstructors)
            ]
        |> P.andThen
            (\( commentsBefore, ( firstConstructor, commentsAfterFirstConstructor ) ) ->
                zeroOrMoreWithSpacesAndCommentsInBetween
                    (P.succeed
                        (\commentsBeforeConstructor ( constructor_, commentsAfter ) commentsBeforePipe ->
                            ( constructor_, commentsAfter )
                        )
                        |. P.keyword (P.Token "|" ExpectingPipe)
                        |= spacesCommentsAndGreaterIndent
                        |= constructor
                    )
                    commentsAfterFirstConstructor
                    |> P.map
                        (Tuple.mapFirst
                            (List.NonEmpty.fromCons firstConstructor)
                        )
            )
        |> P.inContext InConstructors


constructor : Parser_ ( Constructor PossiblyQualified, List Comment )
constructor =
    P.succeed
        (\moduleName_ ( types, commentsAfter ) ->
            ( Declaration.Constructor moduleName_ types
            , commentsAfter
            )
        )
        |= moduleNameWithoutDots
        |= (spacesAndComments
                |> P.andThen typesWithoutUnestedArrow
           )


valueDeclaration : Maybe ( TypeAnnotation, List Comment ) -> Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified, List Comment )
valueDeclaration maybeAnn =
    P.succeed
        (\name commentsAfterVarName ->
            P.oneOf
                [ P.succeed
                    (\commentsBeforeType ( type__, commentsAfter ) ->
                        ( { varName = name
                          , commentsAfterVarName = commentsAfterVarName
                          , commentsBeforeType = commentsBeforeType
                          , type_ = type__
                          }
                        , commentsAfter
                        )
                    )
                    |. P.symbol (P.Token ":" ExpectingColon)
                    |. (case maybeAnn of
                            Nothing ->
                                P.succeed ()

                            Just _ ->
                                P.problem ExpectingTypeAnnotationDefinition
                       )
                    |= spacesCommentsAndGreaterIndent
                    |= type_
                    |> P.inContext InTypeAnnotation
                    |> P.andThen (Just >> valueDeclaration)
                , P.succeed
                    (\commentsExpression ( expr_, commentsAfter ) ->
                        ( name
                        , Declaration.Value
                            { typeAnnotation = Maybe.map Tuple.first maybeAnn
                            , commentsAfterTypeAnnotation =
                                Maybe.map Tuple.second maybeAnn
                                    |> Maybe.withDefault []
                            , expression = expr_
                            }
                        , commentsAfter
                        )
                    )
                    |. P.symbol (P.Token "=" ExpectingEqualsSign)
                    |= spacesCommentsAndGreaterIndent
                    |= expr
                ]
        )
        |= varName
        |= spacesCommentsAndGreaterIndent
        |> P.andThen identity


expr : Parser_ ( LocatedExpr, List Comment )
expr =
    PP.expression
        { oneOf =
            [ if_
            , let_
            , lambda
            , PP.literal (withCommentsAfter literal)
            , PP.literal (withCommentsAfter var)
            , PP.literal (withCommentsAfter unit)
            , list >> withCommentsAfter
            , parenthesized >> withCommentsAfter
            , record >> withCommentsAfter
            , case_
            ]
        , andThenOneOf =
            [ infixLeftWithOperatorResult 1
                (P.succeed identity
                    |. P.symbol (P.Token "++" ExpectingConcatOperator)
                    |= spacesCommentsAndGreaterIndent
                )
                (\( locatedLeft, commentsAfterLeft ) commentsBeforeRight ( locatedRight, commentsAfterRight ) ->
                    ( Located.merge
                        (\left right ->
                            ListConcat
                                { left = left
                                , commentsAfterLeft = commentsAfterLeft
                                , commentsBeforeRight = commentsBeforeRight
                                , right = right
                                }
                        )
                        locatedLeft
                        locatedRight
                    , commentsAfterRight
                    )
                )
            , infixLeftWithOperatorResult 1
                (P.succeed identity
                    |. P.symbol (P.Token "+" ExpectingPlusOperator)
                    |= spacesCommentsAndGreaterIndent
                )
                (\( locatedLeft, commentsAfterLeft ) commentsBeforeRight ( locatedRight, commentsAfterRight ) ->
                    ( Located.merge
                        (\left right ->
                            Plus
                                { left = left
                                , commentsAfterLeft = commentsAfterLeft
                                , commentsBeforeRight = commentsBeforeRight
                                , right = right
                                }
                        )
                        locatedLeft
                        locatedRight
                    , commentsAfterRight
                    )
                )
            , infixRightWithOperatorResult 1
                (P.succeed identity
                    |. P.symbol (P.Token "::" ExpectingConsOperator)
                    |= spacesCommentsAndGreaterIndent
                )
                (\( locatedLeft, commentsAfterLeft ) commentsBeforeRight ( locatedRight, commentsAfterRight ) ->
                    ( Located.merge
                        (\left right ->
                            Cons
                                { left = left
                                , commentsAfterLeft = commentsAfterLeft
                                , commentsBeforeRight = commentsBeforeRight
                                , right = right
                                }
                        )
                        locatedLeft
                        locatedRight
                    , commentsAfterRight
                    )
                )

            -- TODO test this: does `x =\n  call 1\n+ something` work? (it shouldn't: no space before '+')
            , PP.infixLeft 99
                (checkIndent (<) ExpectingIndentation)
                (\( locatedLeft, commentsAfterLeft ) ( locatedRight, commentsAfterRight ) ->
                    ( Located.merge
                        (\left right ->
                            Frontend.Call
                                { fn = left
                                , comments = commentsAfterLeft
                                , argument = right
                                }
                        )
                        locatedLeft
                        locatedRight
                    , commentsAfterRight
                    )
                )
            ]
        , spaces = P.succeed ()
        }
        |> P.inContext InExpr


withCommentsAfter : Parser_ a -> Parser_ ( a, List Comment )
withCommentsAfter parser =
    P.succeed Tuple.pair
        |= parser
        |= spacesAndComments


literal : Parser_ LocatedExpr
literal =
    P.oneOf
        [ literalNumber
        , literalChar
        , literalString
        , literalBool
        ]


literalNumber : Parser_ LocatedExpr
literalNumber =
    let
        parseLiteralNumber =
            P.backtrackable <|
                P.number
                    { int = Ok Int
                    , hex = Ok Int
                    , octal = Err InvalidNumber -- Elm does not support octal notation
                    , binary = Err InvalidNumber -- Elm does not support binary notation
                    , float = Ok Float
                    , invalid = InvalidNumber
                    , expecting = ExpectingNumber
                    }

        negateLiteral toBeNegated =
            case toBeNegated of
                Int int ->
                    Int (negate int)

                Float float ->
                    Float (negate float)

                _ ->
                    toBeNegated
    in
    P.oneOf
        [ P.succeed negateLiteral
            |. P.symbol (P.Token "-" ExpectingMinusSign)
            |= parseLiteralNumber
        , parseLiteralNumber
        ]
        |> P.inContext InNumber
        |> located


type Quotes
    = {- ' -} SingleQuote
    | {- " -} DoubleQuote
    | {- """ -} TripleQuote


isAllowedChar : Quotes -> Char -> Bool
isAllowedChar quotes char =
    case quotes of
        SingleQuote ->
            char /= '\n'

        DoubleQuote ->
            char /= '\n'

        TripleQuote ->
            True


singleQuote : P.Token ParseProblem
singleQuote =
    P.Token "'" ExpectingSingleQuote


doubleQuote : P.Token ParseProblem
doubleQuote =
    P.Token "\"" ExpectingDoubleQuote


tripleQuote : P.Token ParseProblem
tripleQuote =
    P.Token "\"\"\"" ExpectingTripleQuote


character : Quotes -> Parser_ Char
character quotes =
    P.oneOf
        [ -- escaped characters have priority
          P.succeed identity
            |. P.token (P.Token "\\" ExpectingEscapeBackslash)
            |= P.oneOf
                [ P.map (\_ -> '\\') (P.token (P.Token "\\" (ExpectingEscapeCharacter '\\')))
                , P.map (\_ -> '"') (P.token (P.Token "\"" (ExpectingEscapeCharacter '"'))) -- " (elm-vscode workaround)
                , P.map (\_ -> '\'') (P.token (P.Token "'" (ExpectingEscapeCharacter '\'')))
                , P.map (\_ -> '\n') (P.token (P.Token "n" (ExpectingEscapeCharacter 'n')))
                , P.map (\_ -> '\t') (P.token (P.Token "t" (ExpectingEscapeCharacter 't')))
                , P.map (\_ -> '\u{000D}') (P.token (P.Token "r" (ExpectingEscapeCharacter 'r')))
                , P.succeed identity
                    |. P.token (P.Token "u" (ExpectingEscapeCharacter 'u'))
                    |. P.token (P.Token "{" ExpectingLeftBrace)
                    |= unicodeCharacter
                    |. P.token (P.Token "}" ExpectingRightBrace)
                ]
            |> P.inContext InCharEscapeMode
        , -- we don't want to eat the closing delimiter
          (case quotes of
            SingleQuote ->
                P.token singleQuote

            DoubleQuote ->
                P.token doubleQuote

            TripleQuote ->
                P.token tripleQuote
          )
            |> P.andThen (always (P.problem TriedToParseCharacterStoppingDelimiter))
        , -- all other characters (sometimes except newlines)
          P.succeed identity
            |= P.getChompedString (P.chompIf (isAllowedChar quotes) ExpectingChar)
            |> P.andThen
                (\string ->
                    string
                        |> String.uncons
                        |> Maybe.map (Tuple.first >> P.succeed)
                        |> Maybe.withDefault (P.problem (ParseCompilerBug MultipleCharactersChompedInCharacter))
                )
        ]


unicodeCharacter : Parser_ Char
unicodeCharacter =
    P.getChompedString (P.chompWhile Char.isHexDigit)
        |> P.andThen
            (\str ->
                let
                    len =
                        String.length str
                in
                if len < 4 || len > 6 then
                    P.problem InvalidUnicodeCodePoint

                else
                    str
                        |> String.toLower
                        |> Hex.fromString
                        |> Result.map Char.fromCode
                        |> Result.map P.succeed
                        |> Result.withDefault (P.problem InvalidUnicodeCodePoint)
            )
        |> P.inContext InUnicodeCharacter


literalChar : Parser_ LocatedExpr
literalChar =
    P.succeed Char
        |. P.symbol singleQuote
        |= character SingleQuote
        |. P.symbol singleQuote
        |> P.inContext InChar
        |> located


literalString : Parser_ LocatedExpr
literalString =
    P.succeed String
        |= P.oneOf
            [ tripleQuoteString
            , doubleQuoteString
            ]
        |> P.inContext InString
        |> located


doubleQuoteString : Parser_ String
doubleQuoteString =
    P.succeed String.fromList
        |. P.symbol doubleQuote
        |= zeroOrMoreWith (P.succeed ()) (character DoubleQuote)
        |. P.symbol doubleQuote
        |> P.inContext InDoubleQuoteString


tripleQuoteString : Parser_ String
tripleQuoteString =
    P.succeed String.fromList
        |. P.symbol tripleQuote
        |= zeroOrMoreWith (P.succeed ()) (character TripleQuote)
        |. P.symbol tripleQuote
        |> P.inContext InTripleQuoteString


literalBool : Parser_ LocatedExpr
literalBool =
    P.map Bool bool
        |> located


bool : Parser_ Bool
bool =
    P.oneOf
        [ P.map (always True) <| P.keyword (P.Token "True" ExpectingTrue)
        , P.map (always False) <| P.keyword (P.Token "False" ExpectingFalse)
        ]


var : Parser_ LocatedExpr
var =
    P.oneOf
        [ varName
            |> P.map
                (\varName_ ->
                    Frontend.Var
                        { qualifiedness = PossiblyQualified Nothing
                        , name = varName_
                        }
                )
        , qualifiedVar
        ]
        |> located
        |> P.inContext InVar


varName : Parser_ String
varName =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedWords
        , expecting = ExpectingVarName
        }
        |> P.inContext InVarName


qualify : List ModuleName -> PossiblyQualified
qualify modules =
    PossiblyQualified <|
        if List.isEmpty modules then
            Nothing

        else
            Just <| String.join "." modules


qualifiedVar : Parser_ Expr
qualifiedVar =
    P.loop [] qualifiedVarHelp
        |> P.inContext InQualifiedVar


qualifiedVarHelp : List String -> Parser_ (P.Step (List String) Expr)
qualifiedVarHelp acc =
    P.oneOf
        [ P.succeed (\moduleNameStr -> P.Loop (moduleNameStr :: acc))
            |= moduleNameWithoutDots
            |. P.symbol (P.Token "." ExpectingQualifiedVarNameDot)
        , varName
            |> P.map
                (\varName_ ->
                    { qualifiedness = qualify (List.reverse acc)
                    , name = varName_
                    }
                        |> Frontend.Var
                        |> P.Done
                )
        ]


lambda : ExprConfig -> Parser_ ( LocatedExpr, List Comment )
lambda config =
    P.succeed
        (\( startRow, startCol ) ( arguments, commentsAfterArguments ) commentsBeforeBody ( body, commentsAfter ) ->
            ( Frontend.Lambda
                { arguments = arguments
                , commentsAfterArguments = commentsAfterArguments
                , commentsBeforeBody = commentsBeforeBody
                , body =
                    {- Run the promoting transformation on every subexpression,
                       so that after parsing all the arguments aren't unqualified
                       Vars but Arguments.

                       Ie. the lambda parser can't return:

                           -- \x -> x
                           Lambda { argument = VarName "x", body = Var (Nothing, VarName "x") }

                       And instead has to return:

                           -- \x -> x
                           Lambda { argument = VarName "x", body = Argument (VarName "x") }

                       TODO add a fuzz test for this invariant?
                    -}
                    Located.map
                        (Frontend.transform
                            (promoteArguments (List.map .argument arguments))
                        )
                        body
                }
                |> Located.located
                    { start = { row = startRow, col = startCol }
                    , end = .end (Located.getRegion body)
                    }
            , commentsAfter
            )
        )
        |= P.getPosition
        |. P.symbol (P.Token "\\" ExpectingBackslash)
        |= (spacesCommentsAndGreaterIndent
                |> P.andThen (zeroOrMoreWithSpacesAndCommentsInBetween lambdaArgument)
           )
        |. P.keyword (P.Token "->" ExpectingRightArrow)
        |= spacesCommentsAndGreaterIndent
        |= PP.subExpression 0 config
        |> P.inContext InLambda


lambdaArgument : Parser_ (List Comment -> ( { commentsBefore : List Comment, argument : VarName }, List Comment ))
lambdaArgument =
    P.succeed
        (\argument commentsAfter commentsBefore ->
            ( { commentsBefore = commentsBefore
              , argument = argument
              }
            , commentsAfter
            )
        )
        |= varName
        |= spacesCommentsAndGreaterIndent


if_ : ExprConfig -> Parser_ ( LocatedExpr, List Comment )
if_ config =
    P.succeed
        (\( startRow, startCol ) commentsBeforeTest ( test, commentsAfterTest ) commentsBeforeThen ( then_, commentsAfterThen ) commentsBeforeElse ( else_, commentsAfterElse ) ->
            ( Frontend.If
                { commentsBeforeTest = commentsBeforeTest
                , test = test
                , commentsAfterTest = commentsAfterTest
                , commentsBeforeThen = commentsBeforeThen
                , then_ = then_
                , commentsAfterThen = commentsAfterThen
                , commentsBeforeElse = commentsBeforeElse
                , else_ = else_
                }
                |> Located.located
                    { start = { row = startRow, col = startCol }
                    , end = .end (Located.getRegion else_)
                    }
            , commentsAfterElse
            )
        )
        |= P.getPosition
        |. P.keyword (P.Token "if" ExpectingIf)
        |= spacesAndComments
        |= PP.subExpression 0 config
        |. P.keyword (P.Token "then" ExpectingThen)
        |= spacesAndComments
        |= PP.subExpression 0 config
        |. P.keyword (P.Token "else" ExpectingElse)
        |= spacesAndComments
        |= PP.subExpression 0 config
        |> P.inContext InIf


let_ : ExprConfig -> Parser_ ( LocatedExpr, List Comment )
let_ config =
    P.succeed identity
        |= P.getPosition
        |. P.keyword (P.Token "let" ExpectingLet)
        |> P.andThen
            (\( startRow, startCol ) ->
                P.succeed
                    (\commentsBeforeBindings bindingIndent ->
                        ( { row = startRow, col = startCol }
                        , commentsBeforeBindings
                        , bindingIndent
                        )
                    )
                    |= spacesCommentsAndCheckIndent
                        (\_ col -> startCol < col)
                        ExpectingLetIndentation
                    |= P.getCol
            )
        |> P.andThen
            (\( start, commentsBeforeBindings, bindingIndent ) ->
                P.succeed
                    (\( bindings, commentsAfterBindings ) commentsBeforeBody ( body, commentsAfterBody ) ->
                        ( Frontend.Let
                            { bindings = bindings
                            , commentsAfterBindings = commentsAfterBindings
                            , commentsBeforeBody = commentsBeforeBody
                            , body = body
                            }
                            |> Located.located
                                { start = start
                                , end = .end (Located.getRegion body)
                                }
                        , commentsAfterBody
                        )
                    )
                    |= P.withIndent bindingIndent
                        (zeroOrMoreWithSpacesAndCommentsInBetween
                            (letBinding config)
                            commentsBeforeBindings
                        )
                    |. P.keyword (P.Token "in" ExpectingIn)
                    |= spacesCommentsAndGreaterIndent
                    |= PP.subExpression 0 config
            )
        |> P.inContext InLet


letBinding :
    ExprConfig
    -> Parser_ (List Comment -> ( { commentsBefore : List Comment, binding : Binding.Commented LocatedExpr }, List Comment ))
letBinding config =
    P.succeed
        (\( binding_, commentsAfter ) commentsBefore ->
            ( { commentsBefore = commentsBefore
              , binding = binding_
              }
            , commentsAfter
            )
        )
        |. checkIndent (==) ExpectingLetBindingIndentation
        |= binding config
        |> P.inContext InLetBinding


recordBinding : ExprConfig -> Parser_ ( Binding.Commented LocatedExpr, List Comment )
recordBinding config =
    binding config
        |> P.inContext InRecordBinding


binding : ExprConfig -> Parser_ ( Binding.Commented LocatedExpr, List Comment )
binding config =
    P.succeed
        (\name commentsAfterName commentsBeforeBody ( body, commentsAfterBody ) ->
            ( { name = name
              , commentsAfterName = commentsAfterName
              , commentsBeforeBody = commentsBeforeBody
              , body = body
              }
            , commentsAfterBody
            )
        )
        |= varName
        |= spacesCommentsAndGreaterIndent
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |= spacesCommentsAndGreaterIndent
        |= PP.subExpression 0 config


typeBinding : Parser_ ( VarName, ( ConcreteType PossiblyQualified, List Comment ) )
typeBinding =
    P.succeed Tuple.pair
        |= varName
        |. spacesCommentsAndGreaterIndent
        |. P.symbol (P.Token ":" ExpectingColon)
        |. spacesCommentsAndGreaterIndent
        |= P.lazy (\_ -> type_)
        |> P.inContext InTypeBinding


promoteArguments : List VarName -> Expr -> Expr
promoteArguments arguments expr_ =
    -- TODO set of arguments instead of list?
    case expr_ of
        Var var_ ->
            if
                (var_.qualifiedness == PossiblyQualified Nothing)
                    && List.member var_.name arguments
            then
                Argument var_.name

            else
                expr_

        _ ->
            expr_


unit : Parser_ LocatedExpr
unit =
    P.succeed Frontend.Unit
        |. P.keyword (P.Token "()" ExpectingUnit)
        |> P.inContext InUnit
        |> located


parenthesized : ExprConfig -> Parser_ LocatedExpr
parenthesized config =
    P.sequence
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingTupleSeparator
        , end = P.Token ")" ExpectingRightParen
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore ( expr_, commentsAfter ) ->
                    { commentsBefore = commentsBefore
                    , expr = expr_
                    , commentsAfter = commentsAfter
                    }
                )
                |= spacesCommentsAndGreaterIndent
                |= PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> located
        |> P.andThen
            (\items ->
                case Located.unwrap items of
                    [] ->
                        P.problem ExpectingExpression

                    [ item ] ->
                        Located.map
                            (\_ -> Parenthesized item)
                            items
                            |> P.succeed

                    [ item1, item2 ] ->
                        Located.map
                            (\_ -> Tuple item1 item2)
                            items
                            |> P.succeed
                            |> P.inContext InTuple

                    [ item1, item2, item3 ] ->
                        Located.map
                            (\_ -> Tuple3 item1 item2 item3)
                            items
                            |> P.succeed
                            |> P.inContext InTuple

                    _ ->
                        P.problem ExpectingMaxThreeTuple
            )


record : ExprConfig -> Parser_ LocatedExpr
record config =
    P.sequence
        { start = P.Token "{" ExpectingRecordLeftBrace
        , separator = P.Token "," ExpectingRecordSeparator
        , end = P.Token "}" ExpectingRecordRightBrace
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore ( binding_, commentsAfter ) ->
                    { commentsBefore = commentsBefore
                    , binding = binding_
                    , commentsAfter = commentsAfter
                    }
                )
                |= spacesCommentsAndGreaterIndent
                |= recordBinding config
        , trailing = P.Forbidden
        }
        |> P.map Frontend.Record
        |> P.inContext InRecord
        |> located


list : ExprConfig -> Parser_ LocatedExpr
list config =
    P.sequence
        { start = P.Token "[" ExpectingLeftBracket
        , separator = P.Token "," ExpectingListSeparator
        , end = P.Token "]" ExpectingRightBracket
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore ( expr_, commentsAfter ) ->
                    { commentsBefore = commentsBefore
                    , expr = expr_
                    , commentsAfter = commentsAfter
                    }
                )
                |= spacesCommentsAndGreaterIndent
                |= PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> P.map Frontend.List
        |> P.inContext InList
        |> located


case_ : ExprConfig -> Parser_ ( LocatedExpr, List Comment )
case_ config =
    P.succeed
        (\( startRow, startCol ) commentsBeforeTest ( test, commentsAfterTest ) commentsBeforePattern branchAlignCol ->
            zeroOrMoreWithSpacesAndCommentsInBetween (caseBranch config)
                commentsBeforePattern
                |> P.withIndent branchAlignCol
                |> P.map
                    (\( branches, commentsAfter ) ->
                        ( Frontend.Case
                            { commentsBeforeTest = commentsBeforeTest
                            , test = test
                            , commentsAfterTest = commentsAfterTest
                            , branches = branches
                            }
                            |> Located.located
                                { start = { row = startRow, col = startCol }
                                , end =
                                    --TODO: end is the last branch's end
                                    { row = startRow, col = startCol }
                                }
                        , commentsAfter
                        )
                    )
        )
        |= P.getPosition
        |. P.keyword (P.Token "case" ExpectingCase)
        |= spacesAndComments
        |= PP.subExpression 0 config
        |. P.keyword (P.Token "of" ExpectingOf)
        |= spacesAndComments
        |= P.getCol
        |> P.andThen identity
        |> P.inContext InCase


caseBranch :
    ExprConfig
    ->
        Parser_
            (List Comment
             ->
                ( { commentsBeforePattern : List Comment
                  , pattern : LocatedPattern
                  , commentsAfterPattern : List Comment
                  , commentsBeforeBody : List Comment
                  , body : LocatedExpr
                  }
                , List Comment
                )
            )
caseBranch config =
    P.succeed
        (\( pattern_, commentsAfterPattern ) commentsBeforeBody ( body, commentsAfter ) commentsBeforePattern ->
            ( { commentsBeforePattern = commentsBeforePattern
              , pattern = pattern_
              , commentsAfterPattern = commentsAfterPattern
              , commentsBeforeBody = commentsBeforeBody
              , body = body
              }
            , commentsAfter
            )
        )
        |. checkIndent (==) ExpectingIndentation
        |= pattern
        |. checkIndent (<) ExpectingRightArrow
        |. P.symbol (P.Token "->" ExpectingRightArrow)
        |= spacesCommentsAndCheckIndent (<) ExpectingCaseBody
        |= PP.subExpression 0 config


pattern : Parser_ ( LocatedPattern, List Comment )
pattern =
    PP.expression
        { oneOf =
            [ PP.literal (withCommentsAfter patternLiteral)
            , patternTuple >> withCommentsAfter
            , patternList >> withCommentsAfter
            ]
        , andThenOneOf =
            [ infixRightWithOperatorResult 1
                (P.succeed identity
                    |. P.symbol (P.Token "::" ExpectingConsOperator)
                    |= spacesCommentsAndCheckIndent (<) ExpectingIndentation
                )
                (\( locatedLeft, commentsAfterLeft ) commentsBeforeRight ( locatedRight, commentsAfterRight ) ->
                    ( Located.merge
                        (\left right ->
                            PCons
                                { left = left
                                , commentsAfterLeft = commentsAfterLeft
                                , commentsBeforeRight = commentsBeforeRight
                                , right = right
                                }
                        )
                        locatedLeft
                        locatedRight
                    , commentsAfterRight
                    )
                )
            , postfixWithOperatorResult 99
                (P.succeed (\a b c -> ( a, b, c ))
                    |. P.keyword (P.Token "as" ExpectingAsKeyword)
                    |= spacesCommentsAndCheckIndent (<) ExpectingPatternAliasName
                    |= varName
                    |= spacesCommentsAndCheckIndent (<) ExpectingPatternAliasName
                    |> located
                )
                (\( locatedPattern, commentsAfterPattern ) locatedAlias ->
                    let
                        ( commentsBeforeAlias, alias_, commentsAfter ) =
                            Located.unwrap locatedAlias
                    in
                    ( Located.merge
                        (\pattern_ _ ->
                            PAlias
                                { pattern = pattern_
                                , commentsAfterPattern = commentsAfterPattern
                                , commentsBeforeAlias = commentsBeforeAlias
                                , alias = alias_
                                }
                        )
                        locatedPattern
                        locatedAlias
                    , commentsAfter
                    )
                )
            ]
        , spaces = P.succeed ()
        }
        |> P.inContext InPattern


patternLiteral : Parser_ LocatedPattern
patternLiteral =
    P.oneOf
        [ patternAnything
        , patternUnit
        , patternBool
        , patternVar
        , patternChar
        , patternString
        , patternNumber
        , patternRecord
        ]


patternAnything : Parser_ LocatedPattern
patternAnything =
    P.succeed PAnything
        |. P.symbol (P.Token "_" ExpectingPatternAnything)
        |> located


patternUnit : Parser_ LocatedPattern
patternUnit =
    P.succeed PUnit
        |. P.keyword (P.Token "()" ExpectingUnit)
        |> located


patternBool : Parser_ LocatedPattern
patternBool =
    P.map PBool bool
        |> located


patternNumber : Parser_ LocatedPattern
patternNumber =
    let
        parseLiteralNumber =
            P.backtrackable <|
                P.number
                    { int = Ok PInt
                    , hex = Ok PInt
                    , octal = Err InvalidNumber -- Elm does not support octal notation
                    , binary = Err InvalidNumber -- Elm does not support binary notation
                    , float = Ok PFloat
                    , invalid = InvalidNumber
                    , expecting = ExpectingNumber
                    }

        negateLiteral toBeNegated =
            case toBeNegated of
                PInt int ->
                    PInt (negate int)

                PFloat float ->
                    PFloat (negate float)

                _ ->
                    toBeNegated
    in
    P.oneOf
        [ P.succeed negateLiteral
            |. P.symbol (P.Token "-" ExpectingMinusSign)
            |= parseLiteralNumber
        , parseLiteralNumber
        ]
        |> P.inContext InNumber
        |> located


patternChar : Parser_ LocatedPattern
patternChar =
    P.succeed PChar
        |. P.symbol singleQuote
        |= character SingleQuote
        |. P.symbol singleQuote
        |> P.inContext InChar
        |> located


patternString : Parser_ LocatedPattern
patternString =
    P.succeed PString
        |= P.oneOf
            [ tripleQuoteString
            , doubleQuoteString
            ]
        |> P.inContext InString
        |> located


patternVar : Parser_ LocatedPattern
patternVar =
    P.map PVar varName
        |> located
        |> P.inContext InPatternVar


patternTuple : PatternConfig -> Parser_ LocatedPattern
patternTuple config =
    P.sequence
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingTupleSeparator
        , end = P.Token ")" ExpectingRightParen
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore ( pattern_, commentsAfter ) ->
                    { commentsBefore = commentsBefore
                    , pattern = pattern_
                    , commentsAfter = commentsAfter
                    }
                )
                |= spacesCommentsAndGreaterIndent
                |= PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> located
        |> P.andThen
            (\items ->
                case Located.unwrap items of
                    [] ->
                        P.problem ExpectingExpression

                    [ item ] ->
                        Located.map
                            (\_ -> PParenthesized item)
                            items
                            |> P.succeed

                    [ item1, item2 ] ->
                        Located.map
                            (\_ -> PTuple item1 item2)
                            items
                            |> P.succeed
                            |> P.inContext InTuple

                    [ item1, item2, item3 ] ->
                        Located.map
                            (\_ -> PTuple3 item1 item2 item3)
                            items
                            |> P.succeed
                            |> P.inContext InTuple

                    _ ->
                        P.problem ExpectingMaxThreeTuple
            )


patternRecord : Parser_ LocatedPattern
patternRecord =
    P.sequence
        { start = P.Token "{" ExpectingRecordLeftBrace
        , separator = P.Token "," ExpectingRecordSeparator
        , end = P.Token "}" ExpectingRecordRightBrace
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore varName_ commentsAfter ->
                    { commentsBefore = commentsBefore
                    , varName = varName_
                    , commentsAfter = commentsAfter
                    }
                )
                |= spacesCommentsAndGreaterIndent
                |= varName
                |= spacesCommentsAndGreaterIndent
        , trailing = P.Forbidden
        }
        |> P.map PRecord
        |> P.inContext InPatternRecord
        |> located


patternList : PatternConfig -> Parser_ LocatedPattern
patternList config =
    P.sequence
        { start = P.Token "[" ExpectingLeftBracket
        , separator = P.Token "," ExpectingListSeparator
        , end = P.Token "]" ExpectingRightBracket
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore ( pattern_, commentsAfter ) ->
                    { commentsBefore = commentsBefore
                    , pattern = pattern_
                    , commentsAfter = commentsAfter
                    }
                )
                |= spacesCommentsAndGreaterIndent
                |= PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> P.map PList
        |> P.inContext InList
        |> located


type_ : Parser_ ( ConcreteType PossiblyQualified, List Comment )
type_ =
    PP.expression
        { oneOf = typeOneOf
        , andThenOneOf =
            [ infixRightWithOperatorResult 1
                (P.succeed identity
                    |. P.token (P.Token "->" ExpectingRightArrow)
                    |= spacesAndComments
                )
                (\( from, commentsAfterFrom ) commentsBeforeTo ( to, commentsAfterTo ) ->
                    ( ConcreteType.Function
                        { from = from
                        , to = to
                        }
                    , commentsAfterTo
                    )
                )
            ]
        , spaces = P.succeed ()
        }
        |> P.inContext InType


typeWithoutUnestedArrow : Parser_ ( ConcreteType PossiblyQualified, List Comment )
typeWithoutUnestedArrow =
    PP.expression
        { oneOf = typeOneOf
        , andThenOneOf = []
        , spaces = P.succeed ()
        }
        |> P.inContext InType


typeOneOf : List (TypeConfig -> Parser_ ( ConcreteType PossiblyQualified, List Comment ))
typeOneOf =
    [ PP.literal (withCommentsAfter varType)
    , simpleType "()" ConcreteType.Unit
    , simpleType "Bool" ConcreteType.Bool
    , simpleType "Int" ConcreteType.Int
    , simpleType "Float" ConcreteType.Float
    , simpleType "Char" ConcreteType.Char
    , simpleType "String" ConcreteType.String
    , PP.literal listType
    , PP.literal (withCommentsAfter parenthesizedType)
    , PP.literal (withCommentsAfter recordType)
    , PP.literal userDefinedType
    ]


typesWithoutUnestedArrow : List Comment -> Parser_ ( List (ConcreteType PossiblyQualified), List Comment )
typesWithoutUnestedArrow commentsBefore =
    P.loop ( [], commentsBefore ) typesWithoutUnestedArrowHelp


typesWithoutUnestedArrowHelp : ( List (ConcreteType PossiblyQualified), List Comment ) -> Parser_ (P.Step ( List (ConcreteType PossiblyQualified), List Comment ) ( List (ConcreteType PossiblyQualified), List Comment ))
typesWithoutUnestedArrowHelp ( acc, commentsBefore {- TODO -} ) =
    P.oneOf
        [ {- consider `x : Foo.Bar\nx = 1` vs `x : Foo.Bar\n x`

             Right now we've chomped the `Bar` and we may want to chomp
             some arguments.

             If the current indent if greater or equal the current column,
             it means that it will start the `x = ...` declaration.
             This first parser will succeed and return the accumulation.

          -}
          checkIndent (>=) ExpectingIndentation
            |> P.map (\_ -> P.Done ( List.reverse acc, commentsBefore ))
        , {- The next thing to try is a continuation of the type
             annotation - custom type args!
          -}
          typeWithoutUnestedArrow
            |> P.map
                (\( subExpression, commentsAfter ) ->
                    P.Loop ( subExpression :: acc, commentsAfter )
                )
        , {- If the subExpression fails, can be a `->` so we are done with this type -}
          P.Done ( List.reverse acc, commentsBefore )
            |> P.succeed
        ]


varType : Parser_ (ConcreteType PossiblyQualified)
varType =
    P.succeed ConcreteType.TypeVar
        |= varName
        |> P.inContext InTypeVarType


simpleType : String -> ConcreteType PossiblyQualified -> TypeConfig -> Parser_ ( ConcreteType PossiblyQualified, List Comment )
simpleType name parsedType _ =
    P.succeed (Tuple.pair parsedType)
        |. P.keyword (P.Token name (ExpectingSimpleType name))
        |= spacesAndComments


listType : Parser_ ( ConcreteType PossiblyQualified, List Comment )
listType =
    P.succeed (Tuple.mapFirst ConcreteType.List)
        |. P.keyword (P.Token "List" ExpectingListType)
        |. spacesCommentsAndGreaterIndent
        |= P.lazy (\_ -> typeWithoutUnestedArrow)


parenthesizedType : Parser_ (ConcreteType PossiblyQualified)
parenthesizedType =
    P.sequence
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingTupleSeparator
        , end = P.Token ")" ExpectingRightParen
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore ( type__, commentsAfter ) ->
                    --TODO
                    --{ commentsBefore = commentsBefore
                    --, item = item
                    --, commentsAfter = commentsAfter
                    --}
                    type__
                )
                |= spacesCommentsAndGreaterIndent
                |= P.lazy (\_ -> type_)
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\types ->
                case types of
                    [] ->
                        P.problem ExpectingExpression

                    [ type1 ] ->
                        P.succeed type1
                            |> P.inContext InParenthesizedType

                    [ type1, type2 ] ->
                        ConcreteType.Tuple type1 type2
                            |> P.succeed
                            |> P.inContext InTuple

                    [ type1, type2, type3 ] ->
                        ConcreteType.Tuple3 type1 type2 type3
                            |> P.succeed
                            |> P.inContext InTuple3

                    _ ->
                        P.problem ExpectingMaxThreeTuple
            )


recordType : Parser_ (ConcreteType PossiblyQualified)
recordType =
    P.sequence
        { start = P.Token "{" ExpectingLeftBrace
        , separator = P.Token "," ExpectingComma
        , end = P.Token "}" ExpectingRightBrace
        , spaces = P.succeed ()
        , item =
            P.succeed
                (\commentsBefore ( name, ( type__, commentsAfter ) ) ->
                    --TODO
                    --{ commentsBefore = commentsBefore
                    --, item = item
                    --, commentsAfter = commentsAfter
                    --}
                    ( name, type__ )
                )
                |= spacesCommentsAndGreaterIndent
                |= typeBinding
        , trailing = P.Forbidden
        }
        |> P.map (Dict.fromList >> ConcreteType.Record)


{-| Examples:

  - Maybe a
  - List Int
  - Result Foo.Bar
  - Browser.Position Int
  - MyModule.MyDataStructure

-}
userDefinedType : Parser_ ( ConcreteType PossiblyQualified, List Comment )
userDefinedType =
    P.succeed
        (\( modules, name ) ( args, commentsAfter ) ->
            ( ConcreteType.UserDefinedType
                { qualifiedness = qualify modules
                , name = name
                , args = args
                }
            , commentsAfter
            )
        )
        |= qualifiersAndTypeName
        |= (spacesAndComments
                |> P.andThen typesWithoutUnestedArrow
           )
        |> P.inContext InUserDefinedType


qualifiersAndTypeName : Parser_ ( List ModuleName, String )
qualifiersAndTypeName =
    P.loop [] qualifiersAndTypeNameHelp
        |> P.inContext InQualifiersAndTypeName


qualifiersAndTypeNameHelp : List String -> Parser_ (P.Step (List String) ( List ModuleName, String ))
qualifiersAndTypeNameHelp acc =
    P.succeed
        (\moduleOrTypeName continue ->
            if continue then
                P.Loop (moduleOrTypeName :: acc)

            else
                P.Done ( List.reverse acc, moduleOrTypeName )
        )
        |= moduleNameWithoutDots
        |= P.oneOf
            [ P.symbol (P.Token "." ExpectingQualifiedVarNameDot)
                |> P.map (\_ -> True)
            , P.succeed False
            ]



-- Helpers


{-| Parse spaces and comments then check the current defined indentation and the
current column.
-}
spacesCommentsAndCheckIndent : (Int -> Int -> Bool) -> ParseProblem -> Parser_ (List Comment)
spacesCommentsAndCheckIndent check error =
    P.succeed identity
        |= spacesAndComments
        |. checkIndent check error


{-| Parse spaces and comments.
-}
spacesAndComments : Parser_ (List Comment)
spacesAndComments =
    P.succeed identity
        |. spaces
        |= zeroOrMoreWith spaces comment


{-| Parse comment.
-}
comment : Parser_ Comment
comment =
    P.oneOf
        [ P.lineComment (P.Token "--" ExpectingSingleLineCommentStart)
            |> P.getChompedString
            |> located
            |> P.map (\str -> Comment str SingleLine)
        , P.multiComment
            (P.Token "{-" ExpectingMultiLineCommentStart)
            (P.Token "-}" ExpectingMultiLineCommentEnd)
            P.Nestable
            |> P.getChompedString
            |> located
            |> P.map (\str -> Comment str MultiLine)
        ]


{-| Parse zero or more spaces.

It will ignore spaces (' ', '\\n' and '\\r') and raise an error if it finds a tab.

The fact that spaces comes last is very important! It can succeed without
consuming any characters, so if it were the first option, it would always
succeed and bypass the others!

This possibility of success without consumption is also why wee need the
ifProgress helper. It detects if there is no more whitespace to consume.

-}
spaces : Parser_ ()
spaces =
    P.loop 0 <|
        ifProgress <|
            P.oneOf
                [ P.symbol (P.Token "\t" InvalidTab)
                    |> P.andThen (\_ -> P.problem InvalidTab)
                , P.spaces
                ]


{-| Continues a loop if the parser is making progress (consuming anything).

Taken from elm/parser `Parser.multiComment` documentation.

-}
ifProgress : Parser_ a -> Int -> Parser_ (P.Step Int ())
ifProgress parser offset =
    P.succeed identity
        |. parser
        |= P.getOffset
        |> P.map
            (\newOffset ->
                if offset == newOffset then
                    P.Done ()

                else
                    P.Loop newOffset
            )


{-| Check the current indent ([`Parser.getIndent`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#getIndent), previously defined with [`Parser.withIndent`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#withIndent))
and the current column, in this order, with the given function.

If the check function result is `True` it will succeed, otherwise it will return
the given problem.

If no indent is defined, the default indent is `0`.

-}
checkIndent : (Int -> Int -> Bool) -> ParseProblem -> Parser_ ()
checkIndent check error =
    P.succeed
        (\indent col ->
            if check indent col then
                P.succeed ()

            else
                P.problem error
        )
        |= P.getIndent
        |= P.getCol
        |> P.andThen identity


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.

Adapted to behave like \* instead of +.

-}
zeroOrMoreWith : Parser_ () -> Parser_ a -> Parser_ (List a)
zeroOrMoreWith spaces_ p =
    P.loop [] (zeroOrMoreHelp spaces_ p)


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.

Adapted to behave like \* instead of +.

-}
zeroOrMoreHelp : Parser_ () -> Parser_ a -> List a -> Parser_ (P.Step (List a) (List a))
zeroOrMoreHelp spaces_ p vs =
    P.oneOf
        [ P.backtrackable
            (P.succeed (\v -> P.Loop (v :: vs))
                |= p
                |. spaces_
            )
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]


zeroOrMoreWithSpacesAndCommentsInBetween : Parser_ (List Comment -> ( a, List Comment )) -> List Comment -> Parser_ ( List a, List Comment )
zeroOrMoreWithSpacesAndCommentsInBetween parser commentsBefore =
    P.loop ( [], commentsBefore ) (zeroOrMoreWithSpacesAndCommentsInBetweenHelper parser)


zeroOrMoreWithSpacesAndCommentsInBetweenHelper : Parser_ (List Comment -> ( a, List Comment )) -> ( List a, List Comment ) -> Parser_ (P.Step ( List a, List Comment ) ( List a, List Comment ))
zeroOrMoreWithSpacesAndCommentsInBetweenHelper parser ( acc, commentsBefore ) =
    P.oneOf
        [ P.succeed
            (\fn ->
                fn commentsBefore
                    |> Tuple.mapFirst (\i -> i :: acc)
                    |> P.Loop
            )
            |= parser
        , P.succeed (P.Done ( List.reverse acc, commentsBefore ))
        ]


shouldLog : String -> Bool
shouldLog message =
    False


{-| Beware: what this parser logs might sometimes be a lie.

To work it needs to run `Parser.run` inside itself, and it has no way to
set the parser state itself to the state it was called in.

So it runs it with a different source string, zeroed offset, indent, position,
basically with totally different parser state.

Some parsers might not work properly in the "inner" `Parser.run` call as a result
if they depend on this state, and thus might log lies.

Note: the parser itself will still work as before, since we're not resetting the
"outer" `Parser.run` state.

-}
log : String -> Parser_ a -> Parser_ a
log message parser =
    if shouldLog message then
        P.succeed
            (\source offsetBefore ->
                let
                    _ =
                        Debug.log "+++++++++++++++++ starting" message
                in
                ( source, offsetBefore )
            )
            |= P.getSource
            |= P.getOffset
            |> P.andThen
                (\( source, offsetBefore ) ->
                    {- Kinda like that logging decoder from Thoughtbot:
                       https://thoughtbot.com/blog/debugging-dom-event-handlers-in-elm

                       Basically we run `Parser.run` ourselves so that we can
                       get at the context and say something more meaningful
                       about the failure if it happens.
                    -}
                    let
                        remainingSource =
                            String.dropLeft offsetBefore source
                                |> Debug.log "yet to parse  "
                    in
                    let
                        _ =
                            -- the side-effecty part; this might log lies
                            P.run
                                (P.succeed
                                    (\parseResult_ innerOffset ->
                                        let
                                            _ =
                                                Debug.log "chomped string" (String.left innerOffset remainingSource)
                                        in
                                        parseResult_
                                    )
                                    |= parser
                                    |= P.getOffset
                                )
                                remainingSource
                                |> Debug.log "parse result  "
                    in
                    let
                        _ =
                            Debug.log "----------------- ending  " message
                    in
                    parser
                )

    else
        parser


simpleLog : String -> Parser_ a -> Parser_ a
simpleLog msg parser =
    P.succeed
        (\offsetBefore result offsetAfter ->
            let
                _ =
                    Debug.log ("[" ++ msg ++ "] before, after") ( offsetBefore, offsetAfter )
            in
            result
        )
        |= P.getOffset
        |= parser
        |= P.getOffset


checkTooMuchIndentation : String -> Parser_ ()
checkTooMuchIndentation firstSucceedString =
    checkIndent (\indent col -> col == indent + String.length firstSucceedString)
        (TooMuchIndentation firstSucceedString)


spacesCommentsAndGreaterIndent : Parser_ (List Comment)
spacesCommentsAndGreaterIndent =
    spacesCommentsAndCheckIndent (<) ExpectingIndentation


{-| Taken from [dmy/elm-pratt-parser](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt-Advanced#postfix),
made to accept the operator parser result.

It differs from an _infix_ expression by not having left _and_ right expressions.
It has only a left expression and an operator, eg.: 180º (the degree (`º`)
symbol is the postfix operator).

It can be used to parse Elm's aliasing expressions, like `{ foo } as bar`,
since only the `{ foo }` is a pattern expression, but we also need the `bar`
string, which is not another expression.

-}
postfixWithOperatorResult : Int -> Parser c x a -> (e -> a -> e) -> PP.Config c x e -> ( Int, e -> Parser c x e )
postfixWithOperatorResult precedence operator apply _ =
    ( precedence
    , \left -> P.map (apply left) operator
    )


infixLeftWithOperatorResult : Int -> Parser c x a -> (e -> a -> e -> e) -> PP.Config c x e -> ( Int, e -> Parser c x e )
infixLeftWithOperatorResult precedence =
    infixWithOperatorResultHelp ( precedence, precedence )


infixRightWithOperatorResult : Int -> Parser c x a -> (e -> a -> e -> e) -> PP.Config c x e -> ( Int, e -> Parser c x e )
infixRightWithOperatorResult precedence =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixWithOperatorResultHelp ( precedence, precedence - 1 )


infixWithOperatorResultHelp : ( Int, Int ) -> Parser c x a -> (e -> a -> e -> e) -> PP.Config c x e -> ( Int, e -> Parser c x e )
infixWithOperatorResultHelp ( leftPrecedence, rightPrecedence ) operator apply config =
    ( leftPrecedence
    , \left ->
        P.succeed (apply left)
            |= operator
            |= PP.subExpression rightPrecedence config
    )
