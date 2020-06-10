module Stage.Parse.Parser exposing
    ( declaration
    , exposingList
    , expr
    , import_
    , imports
    , moduleDeclaration
    , moduleName
    , module_
    )

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr, LocatedPattern, Pattern(..))
import Elm.Compiler.Error
    exposing
        ( Error(..)
        , ParseContext(..)
        , ParseError(..)
        , ParseProblem(..)
        )
import Elm.Data.Binding exposing (Binding)
import Elm.Data.Declaration as Declaration exposing (Declaration)
import Elm.Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Import exposing (Import)
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Module exposing (Module, ModuleType(..))
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.VarName exposing (VarName)
import Hex
import Parser.Advanced as P exposing ((|.), (|=), Parser)
import Pratt.Advanced as PP
import Set exposing (Set)


type alias Parser_ a =
    Parser ParseContext ParseProblem a


type alias ExprConfig =
    PP.Config ParseContext ParseProblem LocatedExpr


type alias PatternConfig =
    PP.Config ParseContext ParseProblem LocatedPattern


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


module_ : FilePath -> Parser_ (Module LocatedExpr)
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
        |. spacesOnly
        -- TODO check the assumption... does Elm allow newlines there?
        |= moduleName
        |. P.spaces
        |. P.keyword (P.Token "exposing" ExpectingExposingKeyword)
        |. P.spaces
        |= exposingList
        |. newlines


imports : Parser_ (Dict ModuleName Import)
imports =
    P.succeed
        (List.map (\dep -> ( dep.moduleName, dep ))
            >> Dict.fromList
        )
        |= oneOrMoreWith P.spaces import_


import_ : Parser_ Import
import_ =
    P.succeed
        (\moduleName_ as_ exposing_ ->
            { moduleName = moduleName_
            , as_ = as_
            , exposing_ = exposing_
            }
        )
        |. P.keyword (P.Token "import" ExpectingImportKeyword)
        |. spacesOnly
        -- TODO check expectation ... what about newlines here?
        |= moduleName
        |. P.spaces
        |= P.oneOf
            [ P.succeed Just
                |. P.keyword (P.Token "as" ExpectingAsKeyword)
                |. P.spaces
                |= moduleNameWithoutDots
            , P.succeed Nothing
            ]
        |. P.oneOf
            [ -- not sure if this is idiomatic
              P.symbol (P.Token "." ExpectingModuleNameWithoutDots)
                |. P.problem ExpectingModuleNameWithoutDots
            , P.spaces
            ]
        |. P.spaces
        |= P.oneOf
            [ P.succeed Just
                |. P.keyword (P.Token "exposing" ExpectingExposingKeyword)
                |. P.spaces
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


moduleName : Parser_ String
moduleName =
    P.sequence
        { start = P.Token "" (CompilerBug "moduleName start parser failed") -- TODO is this the right way?
        , separator = P.Token "." ExpectingModuleDot
        , end = P.Token "" (CompilerBug "moduleName start parser failed") -- TODO is this the right way?
        , spaces = P.succeed ()
        , item = moduleNameWithoutDots
        , trailing = P.Forbidden
        }
        |> P.andThen
            (\list_ ->
                if List.isEmpty list_ then
                    P.problem ExpectingModuleName

                else
                    P.succeed (String.join "." list_)
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
        { start = P.Token "(" ExpectingExposingListLeftParen
        , separator = P.Token "," ExpectingExposingListSeparatorComma
        , end = P.Token ")" ExpectingExposingListRightParen
        , spaces = P.spaces
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


declarations : Parser_ (List (ModuleName -> Declaration LocatedExpr))
declarations =
    oneOrMoreWith P.spaces
        (P.succeed identity
            |= declaration
            |. P.spaces
        )


declaration : Parser_ (ModuleName -> Declaration LocatedExpr)
declaration =
    P.succeed
        (\name body module__ ->
            { module_ = module__
            , name = name
            , body = body
            }
        )
        |= varName
        |. P.spaces
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |. P.spaces
        {- TODO this only parses Data.Declaration.Value, not TypeAlias or CustomType
           Add parsers for type alises and custom types!
        -}
        |= P.map Declaration.Value expr


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
            , tuple3
            , parenthesizedExpr
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
            , PP.infixLeft 1 (P.symbol (P.Token "++" ExpectingConcatOperator)) (Located.merge ListConcat)
            , PP.infixLeft 1 (P.symbol (P.Token "+" ExpectingPlusOperator)) (Located.merge Plus)
            , PP.infixRight 1 (P.symbol (P.Token "::" ExpectingConsOperator)) (Located.merge Cons)
            ]
        , spaces = P.spaces
        }
        |> P.inContext InExpr


checkNotBeginningOfLine : Parser_ ()
checkNotBeginningOfLine =
    P.getCol
        |> P.andThen
            (\col ->
                if col /= 1 then
                    P.succeed ()

                else
                    P.problem ExpectingNotBeginningOfLine
            )


parenthesizedExpr : ExprConfig -> Parser_ LocatedExpr
parenthesizedExpr config =
    P.succeed identity
        |. P.symbol (P.Token "(" ExpectingLeftParen)
        |= PP.subExpression 0 config
        |. P.symbol (P.Token ")" ExpectingRightParen)


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
                    |. P.token (P.Token "{" ExpectingUnicodeEscapeLeftBrace)
                    |= unicodeCharacter
                    |. P.token (P.Token "}" ExpectingUnicodeEscapeRightBrace)
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
                        |> Maybe.withDefault (P.problem (CompilerBug "Multiple characters chomped in `character`"))
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
        [ P.map
            (\varName_ -> Frontend.Var { module_ = Nothing, name = varName_ })
            varName
        , qualifiedVar
        ]
        |> located


varName : Parser_ String
varName =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedWords
        , expecting = ExpectingVarName
        }


qualifiedVar : Parser_ Expr
qualifiedVar =
    P.sequence
        { start = P.Token "" (CompilerBug "qualifiedVar start parser failed") -- TODO is this the right way?
        , separator = P.Token "." ExpectingQualifiedVarNameDot
        , end = P.Token "" (CompilerBug "qualifiedVar end parser failed") -- TODO is this the right way?
        , spaces = P.succeed ()
        , item = moduleNameWithoutDots
        , trailing = P.Mandatory -- this is the difference from `moduleName`
        }
        |> P.andThen
            (\list_ ->
                let
                    maybeModuleName =
                        if List.isEmpty list_ then
                            Nothing

                        else
                            Just <| String.join "." list_
                in
                P.map
                    (\varName_ -> Frontend.Var { module_ = maybeModuleName, name = varName_ })
                    varName
            )


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
        |. P.symbol (P.Token "\\" ExpectingBackslash)
        |= oneOrMoreWith spacesOnly varName
        |. spacesOnly
        |. P.symbol (P.Token "->" ExpectingRightArrow)
        |. P.spaces
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


let_ : ExprConfig -> Parser_ LocatedExpr
let_ config =
    P.succeed
        (\binding_ body ->
            Frontend.Let
                -- TODO multiple let bindings
                { bindings = [ binding_ ]
                , body = body
                }
        )
        |. P.keyword (P.Token "let" ExpectingLet)
        |. P.spaces
        |= binding config
        |. P.spaces
        |. P.keyword (P.Token "in" ExpectingIn)
        |. P.spaces
        |= PP.subExpression 0 config
        |> P.inContext InLet
        |> located


binding : ExprConfig -> Parser_ (Binding LocatedExpr)
binding config =
    P.succeed Binding
        |= varName
        |. P.spaces
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |. P.spaces
        |= PP.subExpression 0 config
        |> P.inContext InLetBinding


promoteArguments : List VarName -> Expr -> Expr
promoteArguments arguments expr_ =
    -- TODO set of arguments instead of list?
    case expr_ of
        Var var_ ->
            if var_.module_ == Nothing && List.member var_.name arguments then
                Argument var_.name

            else
                expr_

        _ ->
            expr_


unit : ExprConfig -> Parser_ LocatedExpr
unit _ =
    P.succeed Frontend.Unit
        |. P.keyword (P.Token "()" ExpectingUnit)
        |> P.inContext InUnit
        |> located


list : ExprConfig -> Parser_ LocatedExpr
list config =
    P.succeed Frontend.List
        |= P.sequence
            { start = P.Token "[" ExpectingLeftBracket
            , separator = P.Token "," ExpectingListSeparator
            , end = P.Token "]" ExpectingRightBracket
            , spaces = spacesOnly
            , item = PP.subExpression 0 config
            , trailing = P.Forbidden
            }
        |> P.inContext InList
        |> located


tuple : ExprConfig -> Parser_ LocatedExpr
tuple config =
    P.backtrackable
        (P.succeed Tuple
            |. P.symbol (P.Token "(" ExpectingLeftParen)
            |. P.spaces
            |= PP.subExpression 0 config
            |. P.spaces
            |. P.symbol (P.Token "," ExpectingTupleSeparator)
            |. P.spaces
            |= PP.subExpression 0 config
            |. P.spaces
            |. P.symbol (P.Token ")" ExpectingRightParen)
            |> P.inContext InTuple
        )
        |> located


tuple3 : ExprConfig -> Parser_ LocatedExpr
tuple3 config =
    P.backtrackable
        (P.succeed Frontend.Tuple3
            |. P.symbol (P.Token "(" ExpectingLeftParen)
            |. P.spaces
            |= PP.subExpression 0 config
            |. P.spaces
            |. P.symbol (P.Token "," ExpectingTupleSeparator)
            |. P.spaces
            |= PP.subExpression 0 config
            |. P.spaces
            |. P.symbol (P.Token "," ExpectingTupleSeparator)
            |. P.spaces
            |= PP.subExpression 0 config
            |. P.spaces
            |. P.symbol (P.Token ")" ExpectingRightParen)
            |> P.inContext InTuple3
        )
        |> located


record : ExprConfig -> Parser_ LocatedExpr
record config =
    P.succeed Frontend.Record
        |= P.sequence
            { start = P.Token "{" ExpectingRecordLeftBrace
            , separator = P.Token "," ExpectingRecordSeparator
            , end = P.Token "}" ExpectingRecordRightBrace
            , spaces = spacesOnly
            , item = binding config
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
        |. ignorables
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
        , patternString
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


patternBool : Parser_ LocatedPattern
patternBool =
    P.map PBool bool
        |> located


patternVar : Parser_ LocatedPattern
patternVar =
    P.map PVar varName
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


patternRecord : Parser_ LocatedPattern
patternRecord =
    P.sequence
        { start = P.Token "{" ExpectingRecordLeftBrace
        , separator = P.Token "," ExpectingRecordSeparator
        , end = P.Token "}" ExpectingRecordRightBrace
        , spaces = ignorablesAndCheckIndent (<) ExpectingIndentation
        , item = varName
        , trailing = P.Forbidden
        }
        |> P.map PRecord
        |> P.inContext InRecord
        |> located


patternList : PatternConfig -> Parser_ LocatedPattern
patternList config =
    P.sequence
        { start = P.Token "[" ExpectingLeftBracket
        , separator = P.Token "," ExpectingListSeparator
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
        , separator = P.Token "," ExpectingTupleSeparator
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

This possibility of success without consumption is also why wee need the
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
        [ P.backtrackable
            (P.succeed (\v -> P.Loop (v :: vs))
                |= p
                |. spaces
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
