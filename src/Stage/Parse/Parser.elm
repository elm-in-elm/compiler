module Stage.Parse.Parser exposing
    ( declaration
    , exposingList
    , expr
    , import_
    , imports
    , moduleDeclaration
    , moduleName
    , module_
    , typeAnnotation
    , type_
    )

import Dict exposing (Dict)
import Elm.AST.Frontend as Frontend exposing (Expr(..), LocatedExpr)
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
import Elm.Data.Type as Type exposing (Type, TypeOrId(..))
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
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


module_ : FilePath -> Parser_ (Module LocatedExpr TypeAnnotation)
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
        { start = P.Token "(" ExpectingLeftParen
        , separator = P.Token "," ExpectingComma
        , end = P.Token ")" ExpectingRightParen
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


declarations : Parser_ (List (ModuleName -> Declaration LocatedExpr TypeAnnotation))
declarations =
    oneOrMoreWith P.spaces
        (P.succeed identity
            |= declaration
            |. P.spaces
        )


declaration : Parser_ (ModuleName -> Declaration LocatedExpr TypeAnnotation)
declaration =
    P.succeed
        (\typeAnnotation_ name body module__ ->
            { module_ = module__
            , typeAnnotation = typeAnnotation_
            , name = name
            , body = body
            }
        )
        |= P.oneOf
            -- TODO refactor the `backtrackable` away
            -- TODO is it even working correctly?
            [ P.succeed Just
                |= typeAnnotation
                |. P.spaces
                |> P.backtrackable
            , P.succeed Nothing
            ]
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
    P.succeed Bool
        |= P.oneOf
            [ P.map (always True) <| P.keyword (P.Token "True" ExpectingTrue)
            , P.map (always False) <| P.keyword (P.Token "False" ExpectingFalse)
            ]
        |> located


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


typeBinding : Parser_ ( VarName, TypeOrId )
typeBinding =
    P.succeed Tuple.pair
        |= varName
        |. P.spaces
        |. P.symbol (P.Token ":" ExpectingColon)
        |. P.spaces
        |= P.lazy lazyTypeOrId


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
            { start = P.Token "{" ExpectingLeftBrace
            , separator = P.Token "," ExpectingComma
            , end = P.Token "}" ExpectingRightBrace
            , spaces = spacesOnly
            , item = binding config
            , trailing = P.Forbidden
            }
        |> P.inContext InRecord
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


typeAnnotation : Parser_ TypeAnnotation
typeAnnotation =
    -- TODO don't support newline without a space afterward... see the commented out tests
    P.succeed TypeAnnotation
        |= varName
        |. P.spaces
        |. P.symbol (P.Token ":" ExpectingColon)
        |. P.spaces
        |= type_


type_ : Parser_ Type
type_ =
    P.oneOf
        [ varType
        , functionType
        , simpleType "Int" Type.Int
        , simpleType "Float" Type.Float
        , simpleType "Char" Type.Char
        , simpleType "String" Type.String
        , simpleType "Bool" Type.Bool
        , listType
        , simpleType "()" Type.Unit
        , tupleType
        , tuple3Type
        , recordType
        , userDefinedType
        ]


lazyType : () -> Parser_ Type
lazyType () =
    type_


lazyTypeOrId : () -> Parser_ TypeOrId
lazyTypeOrId () =
    P.map Type (lazyType ())


varType : Parser_ Type
varType =
    {- TODO I think we'll need to do `Var String` instead of `Var Int` ...
       and map from user-written strings to Int Var IDs in some later stage
    -}
    {-
       varName
           |> P.getChompedString
           |> P.map Type.Var
    -}
    Debug.todo "varType"


functionType : Parser_ Type
functionType =
    P.succeed (\from to -> Type.Function { from = from, to = to })
        |= P.lazy lazyTypeOrId
        |. spacesOnly
        |. P.keyword (P.Token "->" ExpectingRightArrow)
        |. spacesOnly
        |= P.lazy lazyTypeOrId


simpleType : String -> Type -> Parser_ Type
simpleType name parsedType =
    P.succeed parsedType
        |. P.keyword (P.Token name (ExpectingSimpleType name))


listType : Parser_ Type
listType =
    P.succeed Type.List
        |. P.keyword (P.Token "List" ExpectingListType)
        |. spacesOnly
        |= P.lazy lazyTypeOrId


tupleType : Parser_ Type
tupleType =
    P.succeed Type.Tuple
        |. P.keyword (P.Token "(" ExpectingLeftParen)
        |. spacesOnly
        |= P.lazy lazyTypeOrId
        |. spacesOnly
        |. P.keyword (P.Token "," ExpectingComma)
        |. spacesOnly
        |= P.lazy lazyTypeOrId
        |. spacesOnly
        |. P.keyword (P.Token ")" ExpectingRightParen)


tuple3Type : Parser_ Type
tuple3Type =
    P.succeed Type.Tuple3
        |. P.keyword (P.Token "(" ExpectingLeftParen)
        |. spacesOnly
        |= P.lazy lazyTypeOrId
        |. spacesOnly
        |. P.keyword (P.Token "," ExpectingComma)
        |. spacesOnly
        |= P.lazy lazyTypeOrId
        |. spacesOnly
        |. P.keyword (P.Token "," ExpectingComma)
        |. spacesOnly
        |= P.lazy lazyTypeOrId
        |. spacesOnly
        |. P.keyword (P.Token ")" ExpectingRightParen)


recordType : Parser_ Type
recordType =
    P.succeed (Dict.fromList >> Type.Record)
        |= P.sequence
            { start = P.Token "{" ExpectingLeftBrace
            , separator = P.Token "," ExpectingComma
            , end = P.Token "}" ExpectingRightBrace
            , spaces = spacesOnly -- TODO what about definitions of type aliases etc?
            , item = typeBinding
            , trailing = P.Forbidden
            }


userDefinedType : Parser_ Type
userDefinedType =
    -- Maybe a
    -- List Int
    P.succeed
        (\name args ->
            Type.UserDefinedType
                { name = name
                , args = args
                }
        )
        |= typeOrConstructorName
        |. spacesOnly
        |= zeroOrMoreWith spacesOnly (P.lazy lazyTypeOrId)
