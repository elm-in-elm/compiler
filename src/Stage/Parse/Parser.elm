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
    , portDeclaration
    , spacesOnly
    , typeAliasDeclaration
    , type_
    , valueDeclaration
    )

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Compiler.Error
    exposing
        ( ParseCompilerBug(..)
        , ParseContext(..)
        , ParseProblem(..)
        )
import Elm.Data.Binding as Binding exposing (Binding)
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
    PP.Config ParseContext ParseProblem LocatedExpr


type alias PatternConfig =
    PP.Config ParseContext ParseProblem LocatedPattern


type alias TypeConfig =
    PP.Config ParseContext ParseProblem (ConcreteType PossiblyQualified)


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
        (\( moduleType_, moduleName_, exposing_ ) imports_ declarations_ ->
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
            }
        )
        |= moduleDeclaration
        |. ignorables
        -- TODO what about module doc comment? is it before the imports or after?
        |= imports
        |= declarations
        |> P.inContext (InFile filePath)


moduleDeclaration : Parser_ ( ModuleType, ModuleName, Exposing )
moduleDeclaration =
    P.succeed
        (\moduleType_ moduleName_ exposing_ ->
            ( moduleType_
            , moduleName_
            , exposing_
            )
        )
        |= moduleType
        |. ignorables
        |= notAtBeginningOfLine moduleName
        |. ignorables
        |. notAtBeginningOfLine (P.keyword (P.Token "exposing" ExpectingExposingKeyword))
        |. ignorables
        |= notAtBeginningOfLine exposingList


imports : Parser_ (Dict ModuleName Import)
imports =
    P.succeed
        (List.map (\dep -> ( dep.moduleName, dep ))
            >> Dict.fromList
        )
        |= oneOrMoreWith ignorables import_


import_ : Parser_ Import
import_ =
    P.succeed
        (\moduleName_ as_ exposing_ ->
            { moduleName = moduleName_
            , as_ = as_
            , exposing_ = exposing_
            }
        )
        |. onlyAtBeginningOfLine (P.keyword (P.Token "import" ExpectingImportKeyword))
        |. spacesOnly
        -- TODO check expectation ... what about newlines here?
        |= moduleName
        |. ignorables
        |= P.oneOf
            [ P.succeed Just
                |. P.keyword (P.Token "as" ExpectingAsKeyword)
                |. ignorables
                |= uppercaseNameWithoutDots
            , P.succeed Nothing
            ]
        |. P.oneOf
            [ -- not sure if this is idiomatic
              dot
                |. P.problem ExpectingUppercaseNameWithoutDots
            , ignorables
            ]
        |. ignorables
        |= P.oneOf
            [ P.succeed Just
                |. P.keyword (P.Token "exposing" ExpectingExposingKeyword)
                |. ignorables
                |= exposingList
            , P.succeed Nothing
            ]


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


portModuleType : Parser_ ModuleType
portModuleType =
    P.succeed PortModule
        |. P.keyword (P.Token "port" ExpectingPortKeyword)
        |. spacesOnly
        |. P.keyword (P.Token "module" ExpectingModuleKeyword)


effectModuleType : Parser_ ModuleType
effectModuleType =
    -- TODO some metadata?
    P.succeed EffectModule
        |. P.keyword (P.Token "effect" ExpectingEffectKeyword)
        |. spacesOnly
        |. P.keyword (P.Token "module" ExpectingModuleKeyword)


dot : Parser_ ()
dot =
    P.symbol dot_


dot_ : P.Token ParseProblem
dot_ =
    P.Token "." ExpectingDot


moduleName : Parser_ String
moduleName =
    P.sequence
        { start = P.Token "" (ParseCompilerBug ModuleNameStartParserFailed)
        , separator = dot_
        , end = P.Token "" (ParseCompilerBug ModuleNameEndParserFailed)
        , spaces = P.succeed ()
        , item = uppercaseNameWithoutDots
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\list_ ->
                if List.isEmpty list_ then
                    P.problem ExpectingModuleName

                else
                    P.succeed (String.join "." list_)
            )


nameWithoutDots : Parser_ ( NameType, String )
nameWithoutDots =
    P.oneOf
        [ P.map (Tuple.pair UppercaseName) uppercaseNameWithoutDots
        , P.map (Tuple.pair LowercaseName) varName
        ]


uppercaseNameWithoutDots : Parser_ String
uppercaseNameWithoutDots =
    P.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        , expecting = ExpectingUppercaseNamePart
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
        , spaces = ignorables
        , item = exposedItem
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


declarations : Parser_ (List (ModuleName -> Declaration LocatedExpr TypeAnnotation PossiblyQualified))
declarations =
    P.loop []
        (\decls ->
            P.oneOf
                [ P.succeed (\decl -> P.Loop (decl :: decls))
                    |= declaration
                    |. ignorables
                , P.end ExpectingEnd
                    |> P.map (\() -> P.Done (List.reverse decls))
                ]
        )


declaration : Parser_ (ModuleName -> Declaration LocatedExpr TypeAnnotation PossiblyQualified)
declaration =
    P.succeed
        (\( name, body ) module__ ->
            { module_ = module__
            , name = name
            , body = body
            }
        )
        |= declarationBody
        |> P.inContext InDeclaration


declarationBody : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
declarationBody =
    P.oneOf
        [ typeAliasDeclaration
        , customTypeDeclaration
        , valueDeclaration
        , portDeclaration
        ]


valueDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
valueDeclaration =
    P.succeed
        (\annotationOrDeclName maybeAnnotation expr_ ->
            case maybeAnnotation of
                Nothing ->
                    ( annotationOrDeclName
                    , Declaration.Value
                        { typeAnnotation = Nothing
                        , expression = expr_
                        }
                    )

                Just ( type__, declName ) ->
                    ( declName
                    , Declaration.Value
                        { typeAnnotation =
                            Just
                                { varName = annotationOrDeclName
                                , type_ = type__
                                }
                        , expression = expr_
                        }
                    )
        )
        |= onlyAtBeginningOfLine varName
        |. ignorables
        |= P.oneOf
            [ P.succeed (\type__ declarationName -> Just ( type__, declarationName ))
                |. notAtBeginningOfLine colon
                |. ignorables
                |= notAtBeginningOfLine type_
                |. ignorables
                |= onlyAtBeginningOfLine varName
                |. ignorables
            , P.succeed Nothing
            ]
        |. notAtBeginningOfLine equals
        |. ignorables
        |= expr
        |> P.inContext InValueDeclaration


equals : Parser_ ()
equals =
    P.symbol (P.Token "=" ExpectingEqualsSign)


{-|

     type alias X = Int
     type alias X a = Maybe a

More generally,

     type alias <UserDefinedType> <VarType>* = <Type>

-}
typeAliasDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
typeAliasDeclaration =
    P.succeed
        (\name parameters type__ ->
            ( name
            , Declaration.TypeAlias
                { parameters = parameters
                , definition = type__
                }
            )
        )
        |. onlyAtBeginningOfLine (P.keyword (P.Token "type alias" ExpectingTypeAlias))
        |. ignorables
        |= uppercaseNameWithoutDots
        |. P.symbol (P.Token " " ExpectingSpace)
        |. ignorables
        |= zeroOrMoreWith ignorables varName
        |. ignorables
        |. equals
        |. ignorables
        |= type_
        |> P.inContext InTypeAlias


{-|

     type X = Foo | Bar
     type X a = Foo a | Bar String

More generally,

     type <UserDefinedType> <VarType>* = <Constructor>[ | <Constructor>]*

     Constructor := <Name>[ <Type>]*

-}
customTypeDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
customTypeDeclaration =
    P.succeed
        (\name parameters constructors_ ->
            ( name
            , Declaration.CustomType
                { parameters = parameters
                , constructors = constructors_
                }
            )
        )
        |. P.keyword (P.Token "type" ExpectingTypeAlias)
        |. ignorables
        |= uppercaseNameWithoutDots
        |. P.oneOf
            [ P.symbol (P.Token " " ExpectingSpace)
            , P.symbol (P.Token "\n" ExpectingSpace)
            ]
        |. ignorables
        |= zeroOrMoreWith ignorables varName
        |. ignorables
        |. equals
        |. ignorables
        |= notAtBeginningOfLine constructors
        |> P.inContext InCustomType


constructors : Parser_ (NonEmpty (Constructor PossiblyQualified))
constructors =
    let
        subsequentConstructorsLoop reversedCtors =
            P.succeed (\x -> x)
                |. ignorables
                |= P.oneOf
                    [ P.succeed (\newCtor -> P.Loop (newCtor :: reversedCtors))
                        |. notAtBeginningOfLine (P.token (P.Token "|" ExpectingPipe))
                        |. ignorables
                        |= notAtBeginningOfLine constructor
                    , P.succeed (P.Done (List.reverse reversedCtors))
                    ]
    in
    P.succeed
        (\first rest ->
            List.NonEmpty.fromCons first rest
        )
        |= constructor
        |= P.loop [] subsequentConstructorsLoop
        |> P.inContext InConstructors


constructor : Parser_ (Constructor PossiblyQualified)
constructor =
    P.succeed Declaration.Constructor
        |= uppercaseNameWithoutDots
        |. ignorables
        |= oneOrMoreWith ignorables (notAtBeginningOfLine type_)


expr : Parser_ LocatedExpr
expr =
    PP.expression
        { oneOf =
            [ if_
            , let_
            , lambda
            , PP.literal literal
            , always varOrConstructorValue
            , list
            , parenStartingExpr
            , record
            , case_
            ]
        , andThenOneOf =
            -- TODO test this: does `x =\n  call 1\n+ something` work? (it shouldn't: no space before '+')
            [ PP.infixLeft 99
                (ignorablesAndCheckIndent (<) ExpectingIndentation)
                (Located.merge
                    (\fn argument ->
                        Frontend.Call
                            { fn = fn
                            , argument = argument
                            }
                    )
                )
            , PP.infixLeft 1
                (checkIndent (<) ExpectingIndentation
                    |> P.andThen (\() -> P.symbol (P.Token "++" ExpectingConcatOperator))
                )
                (Located.merge ListConcat)
            , PP.infixLeft 1
                (checkIndent (<) ExpectingIndentation
                    |> P.andThen (\() -> P.symbol (P.Token "+" ExpectingPlusOperator))
                )
                (Located.merge Plus)
            , PP.infixRight 1
                (checkIndent (<) ExpectingIndentation
                    |> P.andThen (\() -> P.symbol (P.Token "::" ExpectingConsOperator))
                )
                (Located.merge Cons)
            ]
        , spaces = ignorables
        }
        |> P.inContext InExpr


parenStartingExpr : ExprConfig -> Parser_ LocatedExpr
parenStartingExpr config =
    P.succeed identity
        |. leftParen
        |= P.oneOf
            [ P.succeed identity
                |= PP.subExpression 0 config
                |> P.andThen
                    (\e1 ->
                        P.oneOf
                            [ P.succeed identity
                                |. ignorables
                                |. comma
                                |= PP.subExpression 0 config
                                |> P.andThen
                                    (\e2 ->
                                        P.succeed identity
                                            |= P.oneOf
                                                [ -- ("x", "y", "z")
                                                  P.succeed (Frontend.Tuple3 e1 e2)
                                                    |. comma
                                                    |= PP.subExpression 0 config
                                                , -- ("x", "y")
                                                  P.succeed (Frontend.Tuple e1 e2)
                                                ]
                                    )
                            , -- ("x"), parenthesized expr
                              P.succeed (Located.unwrap e1)
                            ]
                    )
            , -- ()
              -- Note that unit can't be written as ( ) - no spaces inside!
              P.succeed Frontend.Unit
            ]
        |. rightParen
        |> located


leftParen : Parser_ ()
leftParen =
    P.symbol (P.Token "(" ExpectingLeftParen)


rightParen : Parser_ ()
rightParen =
    P.symbol (P.Token ")" ExpectingRightParen)


literal : Parser_ LocatedExpr
literal =
    P.oneOf
        [ literalNumber
        , literalChar
        , doubleQuoteStartingLiteral String
        , literalBool
        ]


type alias NumberConfig a =
    { int : Int -> a
    , hex : Int -> a
    , float : Float -> a
    }


number : NumberConfig a -> Parser_ a
number config =
    let
        finalizeFloat : { hasParsedDot : Bool, parsedIntPart : Int } -> Parser_ a
        finalizeFloat { hasParsedDot, parsedIntPart } =
            {- Float is just whatever the integer was plus `.` (if not parsed
               yet) and scientific notation (`e`)
            -}
            P.oneOf
                [ -- dot+decimal digits, ie. 123.5 or 123.5e8
                  P.succeed identity
                    |. (if hasParsedDot then
                            P.succeed ()

                        else
                            dot
                       )
                    |= P.getChompedString (P.chompWhile Char.isDigit)
                    |> P.andThen
                        (\chompedDecimalDigits ->
                            case String.toInt chompedDecimalDigits of
                                Just decimalDigits ->
                                    let
                                        decimalLength =
                                            String.length chompedDecimalDigits

                                        scaledDecimalDigits : Float
                                        scaledDecimalDigits =
                                            {- TODO would it be better to (A) scale the integer
                                               part up and adding non-scaled decimal part
                                               instead of (B) scaling decimal part down and adding
                                               to non-scaled integer part?

                                               Ie. for 123.45:

                                               (A) parsedIntPart = 123
                                                   decimalDigits = 45
                                                   scaledDecimalDigits = 0.45
                                                   intermediate result = 123.45
                                                   ... scientific notation scaling happens ...
                                                   result = ...

                                               (B) parsedIntPart = 123
                                                   decimalDigits = 45
                                                   scaledIntPart = 12300
                                                   intermediate result = 12345
                                                   ... scientific notation scaling happens ...
                                                   result = ...

                                               It seems like (B) would have less floating point
                                               problems?
                                            -}
                                            toFloat decimalDigits * 10 ^ negate (toFloat decimalLength)

                                        floatSoFar : Float
                                        floatSoFar =
                                            toFloat parsedIntPart + scaledDecimalDigits
                                    in
                                    P.oneOf
                                        [ scientificNotation floatSoFar
                                        , P.succeed (config.float floatSoFar)
                                        ]

                                Nothing ->
                                    -- This probably only happens on empty string
                                    P.problem FloatCannotEndWithDecimal
                        )
                , -- no dot+decimal digits, eg. 123e5
                  scientificNotation (toFloat parsedIntPart)
                , -- no dot+decimal digits, no `e`, eg. 123
                  P.succeed (config.int parsedIntPart)
                ]

        scientificNotationE : Parser_ ()
        scientificNotationE =
            P.chompIf (\c -> c == 'e' || c == 'E') ExpectingScientificNotationE

        scientificNotation : Float -> Parser_ a
        scientificNotation floatSoFar =
            P.succeed identity
                |. scientificNotationE
                |= P.oneOf
                    [ -- explicit '+' case
                      P.succeed identity
                        |. P.chompIf (\c -> c == '+') ExpectingScientificNotationPlus
                        |= P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen
                            (finalizeScientificNotation
                                { floatSoFar = floatSoFar
                                , shouldNegate = False
                                }
                            )
                    , -- explicit '-' case
                      P.succeed identity
                        |. P.chompIf (\c -> c == '-') ExpectingScientificNotationMinus
                        |= P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen
                            (finalizeScientificNotation
                                { floatSoFar = floatSoFar
                                , shouldNegate = True
                                }
                            )
                    , -- just a number
                      P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen
                            (finalizeScientificNotation
                                { floatSoFar = floatSoFar
                                , shouldNegate = False
                                }
                            )
                    ]

        finalizeScientificNotation : { floatSoFar : Float, shouldNegate : Bool } -> String -> Parser_ a
        finalizeScientificNotation { floatSoFar, shouldNegate } exponentDigits =
            case String.toInt exponentDigits of
                Nothing ->
                    P.problem ExpectingScientificNotationExponent

                Just exponent ->
                    let
                        floatExponent =
                            toFloat exponent

                        exponent_ =
                            if shouldNegate then
                                negate floatExponent

                            else
                                floatExponent
                    in
                    P.succeed (config.float (floatSoFar * 10 ^ exponent_))
    in
    P.oneOf
        [ P.succeed identity
            |. P.symbol (P.Token "0" ExpectingZero)
            |= P.oneOf
                [ scientificNotationE
                    |> P.andThen (\() -> P.problem IntZeroCannotHaveScientificNotation)
                , P.succeed identity
                    |. P.symbol (P.Token "x" ExpectingLowercaseX)
                    |= P.getChompedString (P.chompWhile Char.isHexDigit)
                    |> P.andThen
                        (\chompedHex ->
                            {- Usage of Hex.fromString saves us from checking
                               whether the string is empty.
                            -}
                            case Hex.fromString (String.toLower chompedHex) of
                                Err _ ->
                                    P.problem (ParseCompilerBug ParsedHexButCouldntConvert)

                                Ok int ->
                                    P.succeed (config.hex int)
                        )
                , P.succeed identity
                    |. dot
                    |= finalizeFloat
                        { hasParsedDot = True
                        , parsedIntPart = 0
                        }
                , P.succeed identity
                    |= P.getChompedString (P.chompWhile Char.isDigit)
                    |> P.andThen
                        (\chompedInt ->
                            if String.isEmpty chompedInt then
                                P.succeed (config.int 0)

                            else
                                P.problem IntCannotStartWithZero
                        )
                , -- TODO is this one needed?
                  P.succeed (config.int 0)
                ]
        , P.succeed identity
            |= P.getChompedString (P.chompWhile Char.isDigit)
            |> P.andThen
                (\chompedInt ->
                    case String.toInt chompedInt of
                        Nothing ->
                            if String.isEmpty chompedInt then
                                P.problem ExpectingNumber

                            else
                                P.problem (ParseCompilerBug ParsedIntButCouldntConvert)

                        Just int ->
                            finalizeFloat
                                { hasParsedDot = False
                                , parsedIntPart = int
                                }
                )
        ]


literalNumber : Parser_ LocatedExpr
literalNumber =
    let
        parseLiteralNumber =
            number
                { int = Int
                , hex = HexInt
                , float = Float
                }

        negateLiteral toBeNegated =
            case toBeNegated of
                Int int ->
                    Int (negate int)

                HexInt int ->
                    HexInt (negate int)

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


type StringType
    = {- ' -} CharString
    | {- " -} NormalString
    | {- """ -} MultilineString


canContinueChompingString : StringType -> Char -> Bool
canContinueChompingString stringType char =
    case stringType of
        CharString ->
            char /= '\''

        NormalString ->
            char /= '"' && char /= '\\'

        MultilineString ->
            -- we'll have to check for the other two double-quotes afterwards
            char /= '"' && char /= '\\'


areChompedCharsOk : StringType -> String -> Bool
areChompedCharsOk stringType string =
    case stringType of
        CharString ->
            {- We could also check for the string only being 1 character long
               but we need to convert from String to Char anyway later so it's
               better done there. See `singleCharacter`.
            -}
            not <| String.contains "\n" string

        NormalString ->
            not <| String.contains "\n" string

        MultilineString ->
            True


apostrophe_ : P.Token ParseProblem
apostrophe_ =
    P.Token "'" ExpectingApostrophe


doubleQuote_ : P.Token ParseProblem
doubleQuote_ =
    P.Token "\"" ExpectingDoubleQuote


apostrophe : Parser_ ()
apostrophe =
    P.symbol apostrophe_


doubleQuote : Parser_ ()
doubleQuote =
    P.symbol doubleQuote_


singleCharacter : StringType -> Parser_ Char
singleCharacter stringType =
    stringContents stringType
        |> P.andThen
            (\chars ->
                case String.toList chars of
                    [ char ] ->
                        P.succeed char

                    _ ->
                        P.problem MoreThanOneCharInApostrophes
            )


backslash : Parser_ ()
backslash =
    P.token (P.Token "\\" ExpectingBackslash)


{-| Will chomp string contents (up to and including the string boundary,
ie. all three double-quotes for a multiline string).
-}
stringContents : StringType -> Parser_ String
stringContents stringType =
    let
        escapedChar : Parser_ String
        escapedChar =
            P.oneOf
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
                |> P.map String.fromChar
                |> P.inContext InCharEscapeMode

        help acc =
            P.oneOf
                [ -- escaped characters have priority
                  P.succeed (\char -> P.Loop (char :: acc))
                    |. backslash
                    |= escapedChar
                , -- all other characters except the quote mode we're in (and sometimes except newlines)
                  P.succeed identity
                    |= P.getChompedString (P.chompWhile (canContinueChompingString stringType))
                    |> P.andThen
                        (\string ->
                            let
                                end =
                                    P.succeed (P.Done (List.reverse (string :: acc)))

                                loopWithExtra extra =
                                    P.succeed (P.Loop (extra :: string :: acc))
                            in
                            if areChompedCharsOk stringType string then
                                case stringType of
                                    CharString ->
                                        P.succeed identity
                                            |. apostrophe
                                            |= end

                                    NormalString ->
                                        P.oneOf
                                            -- was it an escape backslash or a double-quote?
                                            [ P.succeed identity
                                                |. backslash
                                                |= escapedChar
                                                |> P.andThen (\char -> loopWithExtra char)
                                            , P.succeed identity
                                                |. doubleQuote
                                                |= end
                                            ]

                                    MultilineString ->
                                        {- We have to somehow get past whatever
                                           stopped us. For a list of such
                                           characters see `areChompedCharsOk`.

                                           In case of escape backslash we need
                                           to eat it and the escaped char after
                                           it and loop; in case it's a single
                                           double-quote we need add it and loop;
                                           in case of two double-quotes add them
                                           and loop; and in case of three we
                                           need to end without adding them.
                                        -}
                                        P.oneOf
                                            [ P.succeed identity
                                                |. backslash
                                                |= escapedChar
                                                |> P.andThen (\char -> loopWithExtra char)
                                            , P.succeed identity
                                                |. doubleQuote
                                                -- 1st " out of 3
                                                |= P.oneOf
                                                    [ P.succeed identity
                                                        |. doubleQuote
                                                        -- 2nd " out of 3
                                                        |= P.oneOf
                                                            [ P.succeed identity
                                                                |. doubleQuote
                                                                -- 3rd " out of 3! Let's end here
                                                                |= end
                                                            , -- We ended at 2nd ", add them and loop
                                                              loopWithExtra "\"\""
                                                            ]
                                                    , -- We ended at 1st ", add it and loop
                                                      loopWithExtra "\""
                                                    ]
                                            ]

                            else
                                P.problem StringContainedBadCharacters
                        )
                ]
    in
    P.loop [] help
        |> P.map String.concat


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
        |. apostrophe
        |= singleCharacter CharString
        |> P.inContext InChar
        |> located


doubleQuoteStartingLiteral : (String -> a) -> Parser_ (Located a)
doubleQuoteStartingLiteral tagger =
    P.succeed tagger
        |. doubleQuote
        |= P.oneOf
            [ P.succeed identity
                |. doubleQuote
                |= P.oneOf
                    [ -- three double quotes - this has to be a """..."""
                      P.succeed identity
                        |. doubleQuote
                        |= stringContents MultilineString
                        |> P.inContext InThreeDoubleQuotesString
                    , -- two double quotes but not a third one: empty string ""
                      P.succeed ""
                    ]
            , -- one double quote - this has to be a "..."
              P.succeed identity
                |= stringContents NormalString
                |> P.inContext InDoubleQuoteString
            ]
        |> P.inContext InString
        |> located


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


varOrConstructorValue : Parser_ LocatedExpr
varOrConstructorValue =
    let
        -- unqualified constructor OR qualified var OR qualified constructor
        go : ( List String, NameType, String ) -> Parser_ Expr
        go ( modules, nameType, lastName ) =
            P.oneOf
                [ P.succeed (\( newType, newName ) -> ( lastName :: modules, newType, newName ))
                    |. dot
                    |= nameWithoutDots
                    |> P.andThen go
                , let
                    toExpr =
                        case nameType of
                            LowercaseName ->
                                Frontend.Var

                            UppercaseName ->
                                Frontend.ConstructorValue
                  in
                  P.succeed <|
                    toExpr
                        { qualifiedness = qualify <| List.reverse modules
                        , name = lastName
                        }
                ]
    in
    P.oneOf
        [ nonqualifiedVar
        , uppercaseNameWithoutDots
            |> P.andThen (\firstUppercase -> go ( [], UppercaseName, firstUppercase ))
        ]
        |> located
        |> P.inContext InVar


nonqualifiedVar : Parser_ Expr
nonqualifiedVar =
    varName
        |> P.map
            (\varName_ ->
                Frontend.Var
                    { qualifiedness = PossiblyQualified Nothing
                    , name = varName_
                    }
            )
        |> P.inContext InNonqualifiedVar


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


lambda : ExprConfig -> Parser_ LocatedExpr
lambda config =
    P.succeed
        (\arguments body ->
            Frontend.Lambda
                { arguments = arguments
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
                    body
                        |> Located.map
                            (Frontend.transform
                                (promoteArguments arguments)
                            )
                }
        )
        |. backslash
        |= oneOrMoreWith spacesOnly varName
        |. spacesOnly
        |. P.symbol (P.Token "->" ExpectingRightArrow)
        |= PP.subExpression 0 config
        |> P.inContext InLambda
        |> located


if_ : ExprConfig -> Parser_ LocatedExpr
if_ config =
    P.succeed
        (\test then_ else_ ->
            Frontend.If
                { test = test
                , then_ = then_
                , else_ = else_
                }
        )
        |. P.keyword (P.Token "if" ExpectingIf)
        |= PP.subExpression 0 config
        |. P.keyword (P.Token "then" ExpectingThen)
        |= PP.subExpression 0 config
        |. P.keyword (P.Token "else" ExpectingElse)
        |= PP.subExpression 0 config
        |> P.inContext InIf
        |> located


rememberIndentation : Parser_ a -> Parser_ a
rememberIndentation parser =
    P.getCol
        |> P.andThen (\col -> P.withIndent col parser)


let_ : ExprConfig -> Parser_ LocatedExpr
let_ config =
    rememberIndentation
        (P.succeed
            (\bindings body ->
                let
                    bindingNames =
                        List.map .name bindings
                in
                Frontend.Let
                    { bindings =
                        bindings
                            |> List.map
                                (Binding.map
                                    (Located.map
                                        (Frontend.transform
                                            (promoteArguments bindingNames)
                                        )
                                    )
                                )
                    , body =
                        body
                            |> Located.map
                                (Frontend.transform
                                    (promoteArguments bindingNames)
                                )
                    }
            )
            |. P.keyword (P.Token "let" ExpectingLet)
            |. ignorables
            {- Not allowed:

                   let
                   x = 1
                   in
                       2

               The `x` must be more indented than the `let`.
               This `checkIndent` checks that.
            -}
            |. checkIndent (<) ExpectingIndentation
            |= rememberIndentation
                {- Here's another indentation trick for `let`:
                   All the bindings have to start at the same column.
                   So we remember the indentation here and then check it in the
                   `binding` parser.
                -}
                (oneOrMoreWith ignorables (letBinding config))
            |. ignorables
            |. P.keyword (P.Token "in" ExpectingIn)
            |= PP.subExpression 0 config
            |> P.inContext InLet
            |> located
        )


letBinding : ExprConfig -> Parser_ (Binding LocatedExpr)
letBinding config =
    P.succeed Binding
        |. checkIndent (==) ExpectingIndentation
        |= varName
        |. ignorables
        |. notAtBeginningOfLine equals
        |. {- Even though `PP.subExpression` eats whitespace, these `ignorables` are
              important because of the `notAtBeginningOfLine` interplay.
           -}
           ignorables
        |= notAtBeginningOfLine (PP.subExpression 0 config)
        |> P.inContext InLetBinding


recordBinding : ExprConfig -> Parser_ (Binding LocatedExpr)
recordBinding config =
    P.succeed Binding
        |= varName
        |. ignorables
        |. equals
        |= PP.subExpression 0 config
        |> P.inContext InRecordBinding


colon : Parser_ ()
colon =
    P.symbol (P.Token ":" ExpectingColon)


typeBinding : TypeConfig -> Parser_ ( VarName, ConcreteType PossiblyQualified )
typeBinding config =
    P.succeed Tuple.pair
        |= varName
        |. ignorables
        |. colon
        |= PP.subExpression 0 config
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


list : ExprConfig -> Parser_ LocatedExpr
list config =
    P.succeed Frontend.List
        |= P.sequence
            { start = P.Token "[" ExpectingLeftBracket
            , separator = P.Token "," ExpectingComma
            , end = P.Token "]" ExpectingRightBracket
            , spaces = ignorables
            , item = PP.subExpression 0 config
            , trailing = P.Forbidden
            }
        |> P.inContext InList
        |> located


comma : Parser_ ()
comma =
    P.symbol (P.Token "," ExpectingComma)


record : ExprConfig -> Parser_ LocatedExpr
record config =
    P.succeed Frontend.Record
        |= P.sequence
            { start = P.Token "{" ExpectingLeftBrace
            , separator = P.Token "," ExpectingComma
            , end = P.Token "}" ExpectingRightBrace
            , spaces = spacesOnly
            , item = recordBinding config
            , trailing = P.Forbidden
            }
        |> P.inContext InRecord
        |> located


case_ : ExprConfig -> Parser_ LocatedExpr
case_ config =
    P.succeed
        (\test branchAlignCol ->
            P.succeed (Frontend.Case test)
                |= P.withIndent branchAlignCol
                    (oneOrMoreWith ignorables (caseBranch config))
        )
        |. P.keyword (P.Token "case" ExpectingCase)
        |= PP.subExpression 0 config
        |. ignorables
        |. P.keyword (P.Token "of" ExpectingOf)
        |. ignorables
        |= P.getCol
        |> P.andThen identity
        |> P.inContext InCase
        |> located


caseBranch : ExprConfig -> Parser_ { pattern : LocatedPattern, body : LocatedExpr }
caseBranch config =
    P.succeed
        (\pattern_ body ->
            { pattern = pattern_
            , body = body
            }
        )
        |. checkIndent (==) ExpectingIndentation
        |= pattern
        |. ignorablesAndCheckIndent (<) ExpectingRightArrow
        |. P.symbol (P.Token "->" ExpectingRightArrow)
        |. ignorablesAndCheckIndent (<) ExpectingCaseBody
        |= PP.subExpression 0 config


pattern : Parser_ LocatedPattern
pattern =
    PP.expression
        { oneOf =
            [ PP.literal patternLiteral
            , patternList
            , patternTuple
            ]
        , andThenOneOf =
            [ PP.infixRight 1 (P.symbol (P.Token "::" ExpectingConsOperator)) (Located.merge PCons)
            , postfix 1
                (P.succeed identity
                    |. P.keyword (P.Token "as" ExpectingAsKeyword)
                    |. ignorablesAndCheckIndent (<) ExpectingPatternAliasName
                    |= varName
                    |> located
                )
                (Located.merge
                    (\pattern_ alias_ ->
                        PAlias pattern_ (Located.unwrap alias_)
                    )
                )
            ]
        , spaces = ignorables
        }
        |> P.inContext InPattern


patternLiteral : Parser_ LocatedPattern
patternLiteral =
    P.oneOf
        [ patternAnything
        , patternUnit
        , patternVar
        , patternChar
        , doubleQuoteStartingLiteral PString
        , patternBool
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


patternChar : Parser_ LocatedPattern
patternChar =
    P.succeed PChar
        |. apostrophe
        |= singleCharacter CharString
        |> P.inContext InChar
        |> located


patternBool : Parser_ LocatedPattern
patternBool =
    P.map PBool bool
        |> located


patternVar : Parser_ LocatedPattern
patternVar =
    P.map PVar varName
        |> located
        |> P.inContext InPatternVar


patternNumber : Parser_ LocatedPattern
patternNumber =
    let
        parseLiteralNumber =
            -- TODO remove backtrackable, similarly to `literalNumber`
            P.backtrackable <|
                P.number
                    { int = Ok PInt
                    , hex = Ok PHexInt
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


patternRecord : Parser_ LocatedPattern
patternRecord =
    P.sequence
        { start = P.Token "{" ExpectingLeftBrace
        , separator = P.Token "," ExpectingComma
        , end = P.Token "}" ExpectingRightBrace
        , spaces = ignorablesAndCheckIndent (<) ExpectingIndentation
        , item = varName
        , trailing = P.Forbidden
        }
        |> P.map PRecord
        |> P.inContext InPatternRecord
        |> located


patternList : PatternConfig -> Parser_ LocatedPattern
patternList config =
    P.sequence
        { start = P.Token "[" ExpectingLeftBracket
        , separator = P.Token "," ExpectingComma
        , end = P.Token "]" ExpectingRightBracket
        , spaces = ignorablesAndCheckIndent (<) ExpectingIndentation
        , item = PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> P.map PList
        |> P.inContext InList
        |> located


patternTuple : PatternConfig -> Parser_ LocatedPattern
patternTuple config =
    P.sequence
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingComma
        , end = P.Token ")" ExpectingRightParen
        , spaces = ignorablesAndCheckIndent (<) ExpectingIndentation
        , item = PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> located
        |> P.andThen
            (\locatedPattern ->
                case Located.unwrap locatedPattern of
                    [ pattern1, pattern2, pattern3 ] ->
                        Located.map (\_ -> PTuple3 pattern1 pattern2 pattern3)
                            locatedPattern
                            |> P.succeed
                            |> P.inContext InTuple3

                    [ pattern1, pattern2 ] ->
                        Located.map (\_ -> PTuple pattern1 pattern2)
                            locatedPattern
                            |> P.succeed
                            |> P.inContext InTuple

                    [ pattern_ ] ->
                        P.succeed pattern_

                    _ ->
                        P.problem ExpectingMaxThreeTuple
            )



-- Helpers


spacesOnly : Parser_ ()
spacesOnly =
    P.chompWhile ((==) ' ')


newlines : Parser_ ()
newlines =
    P.chompWhile ((==) '\n')


{-| Parse zero or more ignorables Elm code.

It will ignore spaces (' ', '\\n' and '\\r') and raise an error if it finds a tab.

The fact that spaces comes last is very important! It can succeed without
consuming any characters, so if it were the first option, it would always
succeed and bypass the others!

This possibility of success without consumption is also why we need the
ifProgress helper. It detects if there is no more whitespace to consume.

-}
ignorables : Parser_ ()
ignorables =
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


{-| Fail if current column <= 1 (these are 1-based, so 1 is leftmost.)
-}
notAtBeginningOfLine : Parser_ a -> Parser_ a
notAtBeginningOfLine parser =
    P.succeed identity
        |. checkIndent (\_ column -> column > 1) ExpectingIndentation
        |= parser


{-| Fail if current column != 1 (these are 1-based, so 1 is leftmost.)
-}
onlyAtBeginningOfLine : Parser_ a -> Parser_ a
onlyAtBeginningOfLine parser =
    P.succeed identity
        |. checkIndent (\_ column -> column == 1) ExpectingNoIndentation
        |= parser


{-| Parse ignorable code then check the current defined indentation and the
current column.
-}
ignorablesAndCheckIndent : (Int -> Int -> Bool) -> ParseProblem -> Parser_ ()
ignorablesAndCheckIndent check error =
    P.succeed ()
        |. ignorables
        |. checkIndent check error


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.

Adapted to behave like \* instead of +.

-}
zeroOrMoreWith : Parser_ () -> Parser_ a -> Parser_ (List a)
zeroOrMoreWith spaces p =
    P.loop [] (zeroOrMoreHelp spaces p)


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.

Adapted to behave like \* instead of +.

-}
zeroOrMoreHelp : Parser_ () -> Parser_ a -> List a -> Parser_ (P.Step (List a) (List a))
zeroOrMoreHelp spaces p vs =
    P.oneOf
        [ p
            |> P.andThen
                (\v ->
                    let
                        result =
                            P.Loop (v :: vs)
                    in
                    P.oneOf
                        [ P.succeed result
                            |. spaces
                        , P.succeed result
                        ]
                )
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.
-}
oneOrMoreWith : Parser_ () -> Parser_ a -> Parser_ (List a)
oneOrMoreWith spaces p =
    P.loop [] (oneOrMoreHelp spaces p)


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.
-}
oneOrMoreHelp : Parser_ () -> Parser_ a -> List a -> Parser_ (P.Step (List a) (List a))
oneOrMoreHelp spaces p vs =
    P.oneOf
        [ P.succeed (\v -> P.Loop (v :: vs))
            |= p
            |. spaces
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]


type_ : Parser_ (ConcreteType PossiblyQualified)
type_ =
    PP.expression
        { oneOf =
            [ PP.literal varType
            , simpleType "Int" ConcreteType.Int
            , simpleType "Float" ConcreteType.Float
            , simpleType "Char" ConcreteType.Char
            , simpleType "String" ConcreteType.String
            , simpleType "Bool" ConcreteType.Bool
            , parenStartingType
            , listType
            , recordType
            , userDefinedType
            ]
        , andThenOneOf =
            [ functionType
            ]
        , spaces = ignorables
        }
        |> P.inContext InType


parenStartingType : TypeConfig -> Parser_ (ConcreteType PossiblyQualified)
parenStartingType config =
    P.succeed identity
        |. leftParen
        |= P.oneOf
            [ P.succeed identity
                |= PP.subExpression 0 config
                |> P.andThen
                    (\t1 ->
                        P.oneOf
                            [ P.succeed identity
                                |. ignorables
                                |. comma
                                |= PP.subExpression 0 config
                                |> P.andThen
                                    (\t2 ->
                                        P.succeed identity
                                            |= P.oneOf
                                                [ -- ("x", "y", "z")
                                                  P.succeed (ConcreteType.Tuple3 t1 t2)
                                                    |. comma
                                                    |= PP.subExpression 0 config
                                                , -- ("x", "y")
                                                  P.succeed (ConcreteType.Tuple t1 t2)
                                                ]
                                    )
                            , -- ("x"), parenthesized type
                              P.succeed t1
                            ]
                    )
            , -- ()
              -- Note that unit can't be written as ( ) - no spaces inside!
              P.succeed ConcreteType.Unit
            ]
        |. rightParen


functionType :
    TypeConfig
    -> ( Int, ConcreteType PossiblyQualified -> Parser_ (ConcreteType PossiblyQualified) )
functionType =
    PP.infixRight 1
        {- If we only tried `notAtBeginningOfLine (P.token ...)` then it would
           not work correctly for the `x : Int\n-> Int` example: the parser
           would happily stop on the first Int.

           This is because of how our Pratt parsers work: they first
           successfully parse the first `Int` and then *optionally* try the
           `andThenOneOf` part that combines exprs together. If that fails,
           that's fine for them.

           But we'd like it to fail loudly if that `->` is there!
           So we parse `->` if at the beginning of line and then fail on
           purpose. (It's important that we *don't backtrace.* This act of
           parsing `->` there is what differentiates this from the simple
           `notAtBeginningOfLine (P.token ...)`.)

           TODO: maybe we can extract this pattern to some helper function?
        -}
        (P.oneOf
            [ onlyAtBeginningOfLine
                (P.token (P.Token "->" ExpectingRightArrow))
                |> P.andThen (\_ -> P.problem ExpectingIndentation)
            , notAtBeginningOfLine
                (P.token (P.Token "->" ExpectingRightArrow))
            ]
        )
        (\from to ->
            ConcreteType.Function
                { from = from
                , to = to
                }
        )


varType : Parser_ (ConcreteType PossiblyQualified)
varType =
    varName
        |> P.getChompedString
        |> P.map ConcreteType.TypeVar
        |> P.inContext InTypeVarType


simpleType : String -> ConcreteType PossiblyQualified -> TypeConfig -> Parser_ (ConcreteType PossiblyQualified)
simpleType name parsedType config =
    PP.constant
        (P.keyword (P.Token name (ExpectingSimpleType name)))
        parsedType
        config


listType : TypeConfig -> Parser_ (ConcreteType PossiblyQualified)
listType config =
    P.succeed ConcreteType.List
        |. P.keyword (P.Token "List" ExpectingListType)
        |= PP.subExpression 0 config


recordType : TypeConfig -> Parser_ (ConcreteType PossiblyQualified)
recordType config =
    P.succeed (Dict.fromList >> ConcreteType.Record)
        |= P.sequence
            { start = P.Token "{" ExpectingLeftBrace
            , separator = P.Token "," ExpectingComma
            , end = P.Token "}" ExpectingRightBrace
            , spaces = ignorables
            , item = notAtBeginningOfLine (typeBinding config)
            , trailing = P.Forbidden
            }


{-| Examples:

  - Maybe a
  - List Int
  - Result Foo.Bar
  - Browser.Position Int
  - MyModule.MyDataStructure

-}
userDefinedType : TypeConfig -> Parser_ (ConcreteType PossiblyQualified)
userDefinedType config =
    P.succeed
        (\( modules, name ) args ->
            ConcreteType.UserDefinedType
                { qualifiedness = qualify modules
                , name = name
                , args = args
                }
        )
        |= qualifiersAndUppercaseName
        |. spacesOnly
        |= P.oneOf
            [ {- consider `x : Foo.Bar\nx = 1` vs `x : Foo.Bar\n x`

                 Right now we've chomped the `Bar` and we want to chomp some
                 arguments.

                 We have to explicitly check whether the next non-newline char is
                 a space or not.

                 If it is, we have a multi-line type annotation
                 on our hands and the `x` at the end is a type argument.

                 If it isn't, we have finished the type annotation
                 parsing and the argument list for the `Foo.Bar` is empty.
                 And that's this `oneOf` case!
              -}
              P.backtrackable
                (P.succeed []
                    |. spacesOnly
                    |. checkNextCharIs '\n' ExpectingNewlineAfterTypeAnnotation
                    |. newlines
                    |. checkNextCharIsNot ' ' ExpectingNonSpaceAfterTypeAnnotationNewlines
                )
            , {- Here, the next thing to parse isn't the `x = ...` declaration
                 but a continuation of the type annotation - custom type args!
              -}
              zeroOrMoreWith ignorables (notAtBeginningOfLine (PP.subExpression 0 config))
            ]
        |> P.inContext InUserDefinedType


type NameType
    = UppercaseName
    | LowercaseName


qualifiersAndUppercaseName : Parser_ ( List ModuleName, String )
qualifiersAndUppercaseName =
    P.sequence
        { start = P.Token "" (ParseCompilerBug QualifiersStartParserFailed)
        , separator = dot_
        , end = P.Token "" (ParseCompilerBug QualifiersEndParserFailed)
        , spaces = P.succeed ()
        , item = uppercaseNameWithoutDots
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\names ->
                case List.reverse names of
                    typeName :: reversedQualifiers ->
                        P.succeed ( List.reverse reversedQualifiers, typeName )

                    _ ->
                        P.problem ExpectingUppercaseName
            )
        |> P.inContext InQualifiersAndUppercaseName


{-| Taken from [dmy/elm-pratt-parser](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt-Advanced#postfix),
made to accept the operator parser result.

It differs from an _infix_ expression by not having left _and_ right expressions.
It has only a left expression and an operator, eg.: 180º (the degree (`º`)
symbol is the postfix operator).

It can be used to parse Elm's aliasing expressions, like `{ foo } as bar`,
since only the `{ foo }` is a pattern expression, but we also need the `bar`
string, which is not another expression.

-}
postfix : Int -> P.Parser c x a -> (e -> a -> e) -> PP.Config c x e -> ( Int, e -> P.Parser c x e )
postfix precedence operator apply _ =
    ( precedence
    , \left -> P.map (apply left) operator
    )


shouldLog : String -> Bool
shouldLog message =
    True


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
                            source
                                |> String.dropLeft offsetBefore
                                |> String.left 100
                                |> Debug.log "yet to parse  "
                    in
                    let
                        length =
                            String.length remainingSource
                                |> Debug.log "of length"
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


checkNextCharIs : Char -> ParseProblem -> Parser_ ()
checkNextCharIs mandatoryChar problem =
    checkNextChar ((==) mandatoryChar) problem


checkNextCharIsNot : Char -> ParseProblem -> Parser_ ()
checkNextCharIsNot forbiddenChar problem =
    checkNextChar ((/=) forbiddenChar) problem


{-| Likely very inefficient...
-}
checkNextChar : (Char -> Bool) -> ParseProblem -> Parser_ ()
checkNextChar charPredicate problem =
    P.succeed
        (\source offset ->
            case String.uncons (String.slice offset (offset + 1) source) of
                Nothing ->
                    P.problem problem

                Just ( nextChar, _ ) ->
                    if charPredicate nextChar then
                        P.succeed ()

                    else
                        P.problem problem
        )
        |= P.getSource
        |= P.getOffset
        |> P.andThen identity


portDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
portDeclaration =
    P.succeed (\name type__ -> ( name, Declaration.Port type__ ))
        |. P.keyword (P.Token "port" ExpectingPortKeyword)
        |. ignorables
        |= notAtBeginningOfLine varName
        |. ignorables
        |. notAtBeginningOfLine colon
        |. ignorables
        |= notAtBeginningOfLine type_
        |> P.inContext InPortDeclaration
