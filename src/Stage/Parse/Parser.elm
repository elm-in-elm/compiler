module Stage.Parse.Parser exposing
    ( State
    , customTypeDeclaration
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
    , typeAnnotation
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
import Elm.Data.Binding exposing (Binding)
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
import Elm.Data.Module exposing (Comment, CommentKind(..), Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (PossiblyQualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Hex
import List.NonEmpty exposing (NonEmpty)
import Set exposing (Set)
import Stage.Parse.AdvancedWithState as P exposing (Parser)
import Stage.Parse.PrattAdvancedWithState as PP


type alias State =
    { comments : List Comment
    }


type alias Parser_ a =
    Parser ParseContext ParseProblem State a


type alias ExprConfig =
    PP.Config ParseContext ParseProblem State LocatedExpr


type alias PatternConfig =
    PP.Config ParseContext ParseProblem State LocatedPattern


type alias TypeConfig =
    PP.Config ParseContext ParseProblem State (ConcreteType PossiblyQualified)


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
        |> P.keep P.getPosition
        |> P.keep p
        |> P.keep P.getPosition


module_ : FilePath -> Parser_ (Module LocatedExpr TypeAnnotation PossiblyQualified)
module_ filePath =
    P.succeed
        (\( moduleType_, moduleName_, exposing_ ) imports_ declarations_ comments_ ->
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
            , comments = List.reverse comments_
            }
        )
        |> P.ignore spacesAndComments
        |> P.keep moduleDeclaration
        |> P.ignore spacesAndComments
        |> P.keep imports
        |> P.ignore spacesAndComments
        |> P.keep declarations
        |> P.ignore spacesAndComments
        |> P.keep (P.withState (\{ comments } -> P.succeed comments))
        |> P.withIndent 1
        |> P.inContext (InFile filePath)


moduleDeclaration : Parser_ ( ModuleType, ModuleName, Exposing )
moduleDeclaration =
    P.succeed
        (\moduleType_ moduleName_ exposing_ ->
            ( moduleType_, moduleName_, exposing_ )
        )
        |> P.keep moduleType
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep moduleName
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.keyword (P.Token "exposing" ExpectingExposingKeyword))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep exposingList


imports : Parser_ (Dict ModuleName Import)
imports =
    zeroOrMoreWithSpacesAndComments import_
        |> P.map
            (List.map (\dep -> ( dep.moduleName, dep ))
                >> Dict.fromList
            )


import_ : Parser_ Import
import_ =
    P.succeed
        (\moduleName_ as_ exposing_ ->
            { moduleName = moduleName_
            , as_ = as_
            , exposing_ = exposing_
            }
        )
        |> P.ignore (P.keyword (P.Token "import" ExpectingImportKeyword))
        |> P.ignore (checkTooMuchIndentation "import")
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep moduleName
        |> P.ignore spacesAndComments
        |> P.keep
            (P.oneOf
                [ P.succeed Just
                    |> P.ignore (checkIndent (<) ExpectingIndentation)
                    |> P.ignore (P.keyword (P.Token "as" ExpectingAsKeyword))
                    |> P.ignore spacesCommentsAndGreaterIndent
                    |> P.keep moduleNameWithoutDots
                , P.succeed Nothing
                ]
            )
        |> P.ignore
            (P.oneOf
                [ -- not sure if this is idiomatic
                  P.symbol (P.Token "." ExpectingModuleNameWithoutDots)
                    |> P.ignore (P.problem ExpectingModuleNameWithoutDots)
                , spacesAndComments
                ]
            )
        |> P.keep
            (P.oneOf
                [ P.succeed Just
                    |> P.ignore (checkIndent (<) ExpectingIndentation)
                    |> P.ignore (P.keyword (P.Token "exposing" ExpectingExposingKeyword))
                    |> P.ignore spacesCommentsAndGreaterIndent
                    |> P.keep exposingList
                , P.succeed Nothing
                ]
            )


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
        |> P.ignore (P.keyword (P.Token "module" ExpectingModuleKeyword))
        |> P.ignore (checkTooMuchIndentation "module")


portModuleType : Parser_ ModuleType
portModuleType =
    P.succeed PortModule
        |> P.ignore (P.keyword (P.Token "port" ExpectingPortKeyword))
        |> P.ignore (checkTooMuchIndentation "port")
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.keyword (P.Token "module" ExpectingModuleKeyword))


effectModuleType : Parser_ ModuleType
effectModuleType =
    -- TODO some metadata?
    P.succeed EffectModule
        |> P.ignore (P.keyword (P.Token "effect" ExpectingEffectKeyword))
        |> P.ignore (checkTooMuchIndentation "effect")
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.keyword (P.Token "module" ExpectingModuleKeyword))


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
        |> P.keep moduleNameWithoutDots
        |> P.keep
            (P.oneOf
                [ P.symbol (P.Token "." ExpectingModuleDot)
                    |> P.map (\_ -> True)
                , P.succeed False
                ]
            )


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
        , spaces = spacesCommentsAndGreaterIndent
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
        |> P.keep typeOrConstructorName
        |> P.keep
            (P.oneOf
                [ P.succeed True
                    |> P.ignore (P.symbol (P.Token "(..)" ExpectingExposedTypeDoublePeriod))
                , P.succeed False
                ]
            )


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
    zeroOrMoreWithSpacesAndComments declaration


declaration : Parser_ (ModuleName -> Declaration LocatedExpr TypeAnnotation PossiblyQualified)
declaration =
    P.succeed
        (\( name, body ) module__ ->
            { module_ = module__
            , name = name
            , body = body
            }
        )
        |> P.keep declarationBody
        |> P.inContext InDeclaration


declarationBody : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
declarationBody =
    P.oneOf
        [ typeAliasDeclaration
        , customTypeDeclaration
        , valueDeclaration
        ]


valueDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
valueDeclaration =
    P.succeed
        (\annotation name expr_ ->
            ( name
            , Declaration.Value
                { typeAnnotation = annotation
                , expression = expr_
                }
            )
        )
        |> P.keep
            (P.oneOf
                -- TODO refactor the `backtrackable` away
                -- TODO is it even working correctly?
                [ P.backtrackable
                    (P.succeed Just
                        |> P.keep typeAnnotation
                        |> P.ignore P.spaces
                    )
                , P.succeed Nothing
                ]
            )
        |> P.keep varName
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.symbol (P.Token "=" ExpectingEqualsSign))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep expr


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
        |> P.ignore (P.keyword (P.Token "type alias" ExpectingTypeAlias))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep moduleNameWithoutDots
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep (zeroOrMoreWithSpacesAndComments varName)
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.symbol (P.Token "=" ExpectingEqualsSign))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep type_
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
        |> P.ignore (P.keyword (P.Token "type" ExpectingTypeAlias))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep moduleNameWithoutDots
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep (zeroOrMoreWith P.spaces varName)
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.symbol (P.Token "=" ExpectingEqualsSign))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep constructors
        |> P.inContext InCustomType


constructors : Parser_ (NonEmpty (Constructor PossiblyQualified))
constructors =
    P.sequence
        { start = P.Token "" (ParseCompilerBug ConstructorsStartParserFailed)
        , separator = P.Token "|" ExpectingPipe
        , end = P.Token "" (ParseCompilerBug ConstructorsEndParserFailed)
        , spaces = spacesCommentsAndGreaterIndent
        , item = constructor
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\constructors_ ->
                case List.NonEmpty.fromList constructors_ of
                    Nothing ->
                        P.problem EmptyListOfConstructors

                    Just c ->
                        P.succeed c
            )
        |> P.inContext InConstructors


constructor : Parser_ (Constructor PossiblyQualified)
constructor =
    P.succeed Declaration.Constructor
        |> P.keep moduleNameWithoutDots
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep (oneOrMoreWith spacesAndComments type_)


expr : Parser_ LocatedExpr
expr =
    PP.expression
        { oneOf =
            [ if_
            , let_
            , lambda
            , PP.literal literal
            , always var
            , unit
            , list
            , tuple
            , record
            , case_
            ]
        , andThenOneOf =
            -- TODO test this: does `x =\n  call 1\n+ something` work? (it shouldn't: no space before '+')
            [ PP.infixLeft 99
                spacesCommentsAndGreaterIndent
                (Located.merge
                    (\fn argument ->
                        Frontend.Call
                            { fn = fn
                            , argument = argument
                            }
                    )
                )
            , PP.infixLeft 1 (P.symbol (P.Token "++" ExpectingConcatOperator)) (Located.merge ListConcat)
            , PP.infixLeft 1 (P.symbol (P.Token "+" ExpectingPlusOperator)) (Located.merge Plus)
            , PP.infixRight 1 (P.symbol (P.Token "::" ExpectingConsOperator)) (Located.merge Cons)
            ]
        , spaces = spacesAndComments
        }
        |> P.inContext InExpr


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
            |> P.ignore (P.symbol (P.Token "-" ExpectingMinusSign))
            |> P.keep parseLiteralNumber
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
            |> P.ignore (P.token (P.Token "\\" ExpectingEscapeBackslash))
            |> P.keep
                (P.oneOf
                    [ P.map (\_ -> '\\') (P.token (P.Token "\\" (ExpectingEscapeCharacter '\\')))
                    , P.map (\_ -> '"') (P.token (P.Token "\"" (ExpectingEscapeCharacter '"'))) -- " (elm-vscode workaround)
                    , P.map (\_ -> '\'') (P.token (P.Token "'" (ExpectingEscapeCharacter '\'')))
                    , P.map (\_ -> '\n') (P.token (P.Token "n" (ExpectingEscapeCharacter 'n')))
                    , P.map (\_ -> '\t') (P.token (P.Token "t" (ExpectingEscapeCharacter 't')))
                    , P.map (\_ -> '\u{000D}') (P.token (P.Token "r" (ExpectingEscapeCharacter 'r')))
                    , P.succeed identity
                        |> P.ignore (P.token (P.Token "u" (ExpectingEscapeCharacter 'u')))
                        |> P.ignore (P.token (P.Token "{" ExpectingLeftBrace))
                        |> P.keep unicodeCharacter
                        |> P.ignore (P.token (P.Token "}" ExpectingRightBrace))
                    ]
                )
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
            |> P.keep (P.getChompedString (P.chompIf (isAllowedChar quotes) ExpectingChar))
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
        |> P.ignore (P.symbol singleQuote)
        |> P.keep (character SingleQuote)
        |> P.ignore (P.symbol singleQuote)
        |> P.inContext InChar
        |> located


literalString : Parser_ LocatedExpr
literalString =
    P.succeed String
        |> P.keep
            (P.oneOf
                [ tripleQuoteString
                , doubleQuoteString
                ]
            )
        |> P.inContext InString
        |> located


doubleQuoteString : Parser_ String
doubleQuoteString =
    P.succeed String.fromList
        |> P.ignore (P.symbol doubleQuote)
        |> P.keep (zeroOrMoreWith (P.succeed ()) (character DoubleQuote))
        |> P.ignore (P.symbol doubleQuote)
        |> P.inContext InDoubleQuoteString


tripleQuoteString : Parser_ String
tripleQuoteString =
    P.succeed String.fromList
        |> P.ignore (P.symbol tripleQuote)
        |> P.keep (zeroOrMoreWith (P.succeed ()) (character TripleQuote))
        |> P.ignore (P.symbol tripleQuote)
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
            |> P.keep moduleNameWithoutDots
            |> P.ignore (P.symbol (P.Token "." ExpectingQualifiedVarNameDot))
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
                    Located.map (Frontend.transform (promoteArguments arguments)) body
                }
        )
        |> P.ignore (P.symbol (P.Token "\\" ExpectingBackslash))
        |> P.keep (oneOrMoreWith spacesCommentsAndGreaterIndent varName)
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.symbol (P.Token "->" ExpectingRightArrow))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep (PP.subExpression 0 config)
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
        |> P.ignore (P.keyword (P.Token "if" ExpectingIf))
        |> P.keep (PP.subExpression 0 config)
        |> P.ignore (P.keyword (P.Token "then" ExpectingThen))
        |> P.keep (PP.subExpression 0 config)
        |> P.ignore (P.keyword (P.Token "else" ExpectingElse))
        |> P.keep (PP.subExpression 0 config)
        |> P.inContext InIf
        |> located


let_ : ExprConfig -> Parser_ LocatedExpr
let_ config =
    P.succeed identity
        |> P.ignore (P.keyword (P.Token "let" ExpectingLet))
        |> P.keep P.getCol
        |> P.andThen
            (\letIndent ->
                P.succeed identity
                    |> P.ignore
                        (spacesCommentsAndCheckIndent
                            (\_ col -> letIndent - 3 < col)
                            ExpectingLetIndentation
                        )
                    |> P.keep P.getCol
            )
        |> P.andThen
            (\bindingIndent ->
                P.succeed
                    (\bindings body ->
                        Frontend.Let
                            { bindings = bindings
                            , body = body
                            }
                    )
                    |> P.keep
                        (P.withIndent bindingIndent
                            (P.loop [] (letBinding config))
                        )
                    |> P.ignore spacesCommentsAndGreaterIndent
                    |> P.keep (PP.subExpression 0 config)
            )
        |> P.inContext InLet
        |> located


letBinding : ExprConfig -> List (Binding LocatedExpr) -> Parser_ (P.Step (List (Binding LocatedExpr)) (List (Binding LocatedExpr)))
letBinding config bs =
    P.oneOf
        [ P.keyword (P.Token "in" ExpectingIn)
            |> P.map (\_ -> P.Done (List.reverse bs))
        , P.succeed (\b -> P.Loop (b :: bs))
            |> P.ignore (checkIndent (==) ExpectingLetBindingIndentation)
            |> P.keep (binding config)
            |> P.ignore spacesAndComments
        ]
        |> P.inContext InLetBinding


recordBinding : ExprConfig -> Parser_ (Binding LocatedExpr)
recordBinding config =
    P.succeed identity
        |> P.keep (binding config)
        |> P.inContext InRecordBinding


binding : ExprConfig -> Parser_ (Binding LocatedExpr)
binding config =
    P.succeed Binding
        |> P.keep varName
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.symbol (P.Token "=" ExpectingEqualsSign))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep (PP.subExpression 0 config)


typeBinding : TypeConfig -> Parser_ ( VarName, ConcreteType PossiblyQualified )
typeBinding config =
    P.succeed Tuple.pair
        |> P.keep varName
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.symbol (P.Token ":" ExpectingColon))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep (PP.subExpression 0 config)
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


unit : ExprConfig -> Parser_ LocatedExpr
unit _ =
    P.succeed Frontend.Unit
        |> P.ignore (P.keyword (P.Token "()" ExpectingUnit))
        |> P.inContext InUnit
        |> located


list : ExprConfig -> Parser_ LocatedExpr
list config =
    P.succeed Frontend.List
        |> P.keep
            (P.sequence
                { start = P.Token "[" ExpectingLeftBracket
                , separator = P.Token "," ExpectingListSeparator
                , end = P.Token "]" ExpectingRightBracket
                , spaces = spacesCommentsAndGreaterIndent
                , item = PP.subExpression 0 config
                , trailing = P.Forbidden
                }
            )
        |> P.inContext InList
        |> located


tuple : ExprConfig -> Parser_ LocatedExpr
tuple config =
    P.sequence
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingTupleSeparator
        , end = P.Token ")" ExpectingRightParen
        , spaces = spacesCommentsAndGreaterIndent
        , item = PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> located
        |> P.andThen
            (\locatedExpr ->
                case Located.unwrap locatedExpr of
                    [ expr1, expr2, expr3 ] ->
                        Located.map (\_ -> Tuple3 expr1 expr2 expr3)
                            locatedExpr
                            |> P.succeed
                            |> P.inContext InTuple3

                    [ expr1, expr2 ] ->
                        Located.map (\_ -> Tuple expr1 expr2)
                            locatedExpr
                            |> P.succeed
                            |> P.inContext InTuple

                    [ expr_ ] ->
                        P.succeed expr_

                    _ ->
                        P.problem ExpectingMaxThreeTuple
            )


record : ExprConfig -> Parser_ LocatedExpr
record config =
    P.succeed Frontend.Record
        |> P.keep
            (P.sequence
                { start = P.Token "{" ExpectingRecordLeftBrace
                , separator = P.Token "," ExpectingRecordSeparator
                , end = P.Token "}" ExpectingRecordRightBrace
                , spaces = spacesCommentsAndGreaterIndent
                , item = recordBinding config
                , trailing = P.Forbidden
                }
            )
        |> P.inContext InRecord
        |> located


case_ : ExprConfig -> Parser_ LocatedExpr
case_ config =
    P.succeed
        (\test branchAlignCol ->
            P.succeed (Frontend.Case test)
                |> P.keep
                    (P.withIndent
                        branchAlignCol
                        (oneOrMoreWith spacesAndComments (caseBranch config))
                    )
        )
        |> P.ignore (P.keyword (P.Token "case" ExpectingCase))
        |> P.ignore spacesAndComments
        |> P.keep (PP.subExpression 0 config)
        |> P.ignore spacesAndComments
        |> P.ignore (P.keyword (P.Token "of" ExpectingOf))
        |> P.ignore spacesAndComments
        |> P.keep P.getCol
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
        |> P.ignore (checkIndent (==) ExpectingIndentation)
        |> P.keep pattern
        |> P.ignore (spacesCommentsAndCheckIndent (<) ExpectingRightArrow)
        |> P.ignore (P.symbol (P.Token "->" ExpectingRightArrow))
        |> P.ignore (spacesCommentsAndCheckIndent (<) ExpectingCaseBody)
        |> P.keep (PP.subExpression 0 config)


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
            , PP.postfixWithOperatorResult 1
                (P.succeed identity
                    |> P.ignore (P.keyword (P.Token "as" ExpectingAsKeyword))
                    |> P.ignore (spacesCommentsAndCheckIndent (<) ExpectingPatternAliasName)
                    |> P.keep varName
                    |> located
                )
                (Located.merge
                    (\pattern_ alias_ ->
                        PAlias pattern_ (Located.unwrap alias_)
                    )
                )
            ]
        , spaces = spacesAndComments
        }
        |> P.inContext InPattern


patternLiteral : Parser_ LocatedPattern
patternLiteral =
    P.oneOf
        [ patternAnything
        , patternUnit
        , patternVar
        , patternChar
        , patternString
        , patternBool
        , patternNumber
        , patternRecord
        ]


patternAnything : Parser_ LocatedPattern
patternAnything =
    P.succeed PAnything
        |> P.ignore (P.symbol (P.Token "_" ExpectingPatternAnything))
        |> located


patternUnit : Parser_ LocatedPattern
patternUnit =
    P.succeed PUnit
        |> P.ignore (P.keyword (P.Token "()" ExpectingUnit))
        |> located


patternChar : Parser_ LocatedPattern
patternChar =
    P.succeed PChar
        |> P.ignore (P.symbol singleQuote)
        |> P.keep (character SingleQuote)
        |> P.ignore (P.symbol singleQuote)
        |> P.inContext InChar
        |> located


patternString : Parser_ LocatedPattern
patternString =
    P.succeed PString
        |> P.keep
            (P.oneOf
                [ tripleQuoteString
                , doubleQuoteString
                ]
            )
        |> P.inContext InString
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
            |> P.ignore (P.symbol (P.Token "-" ExpectingMinusSign))
            |> P.keep parseLiteralNumber
        , parseLiteralNumber
        ]
        |> P.inContext InNumber
        |> located


patternRecord : Parser_ LocatedPattern
patternRecord =
    P.sequence
        { start = P.Token "{" ExpectingRecordLeftBrace
        , separator = P.Token "," ExpectingRecordSeparator
        , end = P.Token "}" ExpectingRecordRightBrace
        , spaces = spacesCommentsAndGreaterIndent
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
        , separator = P.Token "," ExpectingListSeparator
        , end = P.Token "]" ExpectingRightBracket
        , spaces = spacesCommentsAndGreaterIndent
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
        , separator = P.Token "," ExpectingTupleSeparator
        , end = P.Token ")" ExpectingRightParen
        , spaces = spacesCommentsAndGreaterIndent
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


{-| Parse spaces and comments then check the current defined indentation and the
current column.
-}
spacesCommentsAndCheckIndent : (Int -> Int -> Bool) -> ParseProblem -> Parser_ ()
spacesCommentsAndCheckIndent check error =
    P.succeed ()
        |> P.ignore spacesAndComments
        |> P.ignore (checkIndent check error)


{-| Parse spaces and comments.
-}
spacesAndComments : Parser_ ()
spacesAndComments =
    P.succeed ()
        |> P.ignore spaces
        |> P.ignore (zeroOrMoreWith spaces comment)


{-| Parse comment and add to the Parser state.
-}
comment : Parser_ ()
comment =
    P.oneOf
        [ P.lineComment (P.Token "--" ExpectingLeftParen)
            |> P.getChompedString
            |> located
            |> P.map (\str -> Comment str SingleLine)
        , P.multiComment
            (P.Token "{-" ExpectingLeftParen)
            (P.Token "-}" ExpectingLeftParen)
            P.Nestable
            |> P.getChompedString
            |> located
            |> P.map (\str -> Comment str MultiLine)
        ]
        |> P.andThen
            (\cmmnt ->
                P.updateState (\s -> { s | comments = cmmnt :: s.comments })
            )


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
        |> P.ignore parser
        |> P.keep P.getOffset
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
        |> P.keep P.getIndent
        |> P.keep P.getCol
        |> P.andThen identity


{-| Fail if current column <= 1 (these are 1-based, so 1 is leftmost.)
-}
onlyIndented : Parser_ a -> Parser_ a
onlyIndented parser =
    P.succeed identity
        |> P.ignore (checkIndent (\_ column -> column > 1) ExpectingIndentation)
        |> P.keep parser


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
                |> P.keep p
                |> P.ignore spaces_
            )
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.
-}
oneOrMoreWith : Parser_ () -> Parser_ a -> Parser_ (List a)
oneOrMoreWith spaces_ p =
    P.loop [] (oneOrMoreHelp spaces_ p)


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.
-}
oneOrMoreHelp : Parser_ () -> Parser_ a -> List a -> Parser_ (P.Step (List a) (List a))
oneOrMoreHelp spaces_ p vs =
    P.oneOf
        [ P.succeed (\v -> P.Loop (v :: vs))
            |> P.keep p
            |> P.ignore spaces_
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]


typeAnnotation : Parser_ TypeAnnotation
typeAnnotation =
    P.succeed TypeAnnotation
        |> P.keep varName
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.ignore (P.symbol (P.Token ":" ExpectingColon))
        |> P.ignore spacesCommentsAndGreaterIndent
        |> P.keep type_
        |> P.inContext InTypeAnnotation


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
            , simpleType "()" ConcreteType.Unit
            , listType
            , parenthesizedType
            , recordType
            , userDefinedType
            ]
        , andThenOneOf =
            [ PP.infixRight 1
                (P.token (P.Token "->" ExpectingRightArrow))
                (\from to -> ConcreteType.Function { from = from, to = to })
            ]
        , spaces = spacesAndComments
        }
        |> P.inContext InType


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
        |> P.ignore (P.keyword (P.Token "List" ExpectingListType))
        |> P.ignore spacesAndComments
        |> P.keep (PP.subExpression 0 config)


parenthesizedType : TypeConfig -> Parser_ (ConcreteType PossiblyQualified)
parenthesizedType config =
    P.sequence
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingTupleSeparator
        , end = P.Token ")" ExpectingRightParen
        , spaces = spacesCommentsAndGreaterIndent
        , item = PP.subExpression 0 config
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\types ->
                case types of
                    [ type1, type2, type3 ] ->
                        ConcreteType.Tuple3 type1 type2 type3
                            |> P.succeed
                            |> P.inContext InTuple3

                    [ type1, type2 ] ->
                        ConcreteType.Tuple type1 type2
                            |> P.succeed
                            |> P.inContext InTuple

                    [ expr_ ] ->
                        P.succeed expr_
                            |> P.inContext InParenthesizedType

                    _ ->
                        P.problem ExpectingMaxThreeTuple
            )


recordType : TypeConfig -> Parser_ (ConcreteType PossiblyQualified)
recordType config =
    P.sequence
        { start = P.Token "{" ExpectingLeftBrace
        , separator = P.Token "," ExpectingComma
        , end = P.Token "}" ExpectingRightBrace
        , spaces = spacesCommentsAndGreaterIndent -- TODO what about definitions of type aliases etc?
        , item = typeBinding config
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
        |> P.keep qualifiersAndTypeName
        |> P.ignore spacesAndComments
        |> P.keep (P.loop [] (userDefinedTypeHelp config))
        |> P.inContext InUserDefinedType


userDefinedTypeHelp : TypeConfig -> List (ConcreteType PossiblyQualified) -> Parser_ (P.Step (List (ConcreteType PossiblyQualified)) (List (ConcreteType PossiblyQualified)))
userDefinedTypeHelp config acc =
    P.oneOf
        [ {- consider `x : Foo.Bar\nx = 1` vs `x : Foo.Bar\n x`

             Right now we've chomped the `Bar` and we may want to chomp
             some arguments.

             If the current indent if greater or equal the current column,
             it means that it will start the `x = ...` declaration.
             This first parser will succeed and return the accumulation.

          -}
          checkIndent (>=) ExpectingIndentation
            |> P.map (\_ -> P.Done (List.reverse acc))
        , {- The next thing to try is a continuation of the type
             annotation - custom type args!
          -}
          PP.subExpression 0 config
            |> P.map (\subExpression -> P.Loop (subExpression :: acc))
        , {- If the subExpression fails, can be a `->` so we are done with this type -}
          P.Done (List.reverse acc)
            |> P.succeed
        ]


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
        |> P.keep moduleNameWithoutDots
        |> P.keep
            (P.oneOf
                [ P.symbol (P.Token "." ExpectingQualifiedVarNameDot)
                    |> P.map (\_ -> True)
                , P.succeed False
                ]
            )


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
            |> P.keep P.getSource
            |> P.keep P.getOffset
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
                                    |> P.keep parser
                                    |> P.keep P.getOffset
                                )
                                { comments = [] }
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
        |> P.keep P.getOffset
        |> P.keep parser
        |> P.keep P.getOffset


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
        |> P.keep P.getSource
        |> P.keep P.getOffset
        |> P.andThen identity


{-| Taken from Punie/elm-parser-extras (original name: `many`), made to work with
Parser.Advanced.Parser instead of the simple one.
-}
zeroOrMoreWithSpacesAndComments : Parser_ a -> Parser_ (List a)
zeroOrMoreWithSpacesAndComments =
    --TODO: Maybe rename oneOrMore since it accepts zero results?
    oneOrMoreWith spacesAndComments


checkTooMuchIndentation : String -> Parser_ ()
checkTooMuchIndentation firstSucceedString =
    checkIndent (\indent col -> col == indent + String.length firstSucceedString)
        (TooMuchIndentation firstSucceedString)


spacesCommentsAndGreaterIndent : Parser_ ()
spacesCommentsAndGreaterIndent =
    spacesCommentsAndCheckIndent (<) ExpectingIndentation
