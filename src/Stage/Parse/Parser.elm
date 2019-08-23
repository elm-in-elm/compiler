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

import AST.Common.Literal exposing (Literal(..))
import AST.Common.Located as Located exposing (Located)
import AST.Frontend as Frontend exposing (Expr(..), LocatedExpr)
import AssocList as Dict exposing (Dict)
import Data.Binding exposing (Binding)
import Data.Declaration as Declaration exposing (Declaration)
import Data.Exposing exposing (ExposedItem(..), Exposing(..))
import Data.FilePath exposing (FilePath)
import Data.Import exposing (Import)
import Data.Module exposing (Module, ModuleType(..))
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)
import Error
    exposing
        ( Error(..)
        , ParseContext(..)
        , ParseError(..)
        , ParseProblem(..)
        )
import Hex
import Parser.Advanced as P exposing ((|.), (|=), Parser)
import Pratt.Advanced as PP
import Set exposing (Set)


type alias Parser_ a =
    Parser ParseContext ParseProblem a


type alias ExprConfig =
    PP.Config ParseContext ParseProblem LocatedExpr


located : Parser_ p -> Parser_ (Located p)
located p =
    P.succeed Located.parsed
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


moduleDeclaration : Parser_ ( ModuleType, ModuleName, Exposing )
moduleDeclaration =
    P.succeed
        (\moduleType_ moduleName_ exposing_ ->
            ( moduleType_
            , ModuleName.fromString moduleName_
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
            { moduleName = ModuleName.fromString moduleName_
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
            [ P.succeed (ModuleName.fromString >> Just)
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
    -- TODO more metadata?
    P.succeed (EffectModule {})
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
    P.map (ExposedValue << VarName.fromString) varName


exposedTypeAndOptionallyAllConstructors : Parser_ ExposedItem
exposedTypeAndOptionallyAllConstructors =
    P.succeed
        (\name hasDoublePeriod ->
            if hasDoublePeriod then
                ExposedTypeAndAllConstructors name

            else
                ExposedType name
        )
        |= P.map VarName.fromString typeOrConstructorName
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
        |= P.map VarName.fromString varName
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
            ]
        , andThenOneOf =
            -- TODO test this: does `x =\n  call 1\n+ something` work? (it shouldn't: no space before '+')
            [ PP.infixLeft 99
                checkNotBeginningOfLine
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
    P.succeed Literal
        |= P.oneOf
            [ literalNumber
            , literalChar
            , literalString
            , literalBool
            ]
        |> P.inContext InLiteral
        |> located


literalNumber : Parser_ Literal
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


literalChar : Parser_ Literal
literalChar =
    P.succeed Char
        |. P.symbol singleQuote
        |= character SingleQuote
        |. P.symbol singleQuote
        |> P.inContext InChar


literalString : Parser_ Literal
literalString =
    P.succeed String
        |= P.oneOf
            [ tripleQuoteString
            , doubleQuoteString
            ]
        |> P.inContext InString


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


literalBool : Parser_ Literal
literalBool =
    P.succeed Bool
        |= P.oneOf
            [ P.map (always True) <| P.keyword (P.Token "True" ExpectingTrue)
            , P.map (always False) <| P.keyword (P.Token "False" ExpectingFalse)
            ]


var : Parser_ LocatedExpr
var =
    P.oneOf
        [ P.map
            (\varName_ -> Frontend.var Nothing (VarName.fromString varName_))
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
                            Just <| ModuleName.fromString <| String.join "." list_
                in
                P.map
                    (\varName_ -> Frontend.var maybeModuleName <| VarName.fromString varName_)
                    varName
            )


lambda : ExprConfig -> Parser_ LocatedExpr
lambda config =
    P.succeed
        (\arguments body ->
            Frontend.lambda
                arguments
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
                (Located.map (Frontend.transform (promoteArguments arguments)) body)
        )
        |. P.symbol (P.Token "\\" ExpectingBackslash)
        |= oneOrMoreWith spacesOnly (P.map VarName.fromString varName)
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
        |= P.map VarName.fromString varName
        |. P.spaces
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |. P.spaces
        |= PP.subExpression 0 config
        |> P.inContext InLetBinding


promoteArguments : List VarName -> Expr -> Expr
promoteArguments arguments expr_ =
    -- TODO set of arguments instead of list?
    case expr_ of
        Var { qualifier, name } ->
            if qualifier == Nothing && List.member name arguments then
                Argument name

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



-- Helpers


spacesOnly : Parser_ ()
spacesOnly =
    P.chompWhile ((==) ' ')


newlines : Parser_ ()
newlines =
    P.chompWhile ((==) '\n')


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


log : String -> Parser_ a -> Parser_ a
log message parser =
    P.succeed ()
        |> P.andThen
            (\() ->
                let
                    _ =
                        Debug.log "starting" message
                in
                P.succeed
                    (\source offsetBefore parseResult offsetAfter ->
                        let
                            _ =
                                Debug.log "-----------------------------------------------" message

                            _ =
                                Debug.log "source         " source

                            _ =
                                Debug.log "chomped string " (String.slice offsetBefore offsetAfter source)

                            _ =
                                Debug.log "parsed result  " parseResult
                        in
                        parseResult
                    )
                    |= P.getSource
                    |= P.getOffset
                    |= parser
                    |= P.getOffset
            )
