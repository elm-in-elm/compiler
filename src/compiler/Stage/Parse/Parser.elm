module Stage.Parse.Parser exposing
    ( dependencies
    , exposingList
    , expr
    , moduleDeclaration
    , moduleName
    , module_
    )

import AST.Common.Literal exposing (Literal(..))
import AST.Frontend as Frontend exposing (Expr(..))
import Common
import Common.Types
    exposing
        ( Binding
        , Dependency
        , Dict_
        , ExposedItem(..)
        , Exposing(..)
        , FilePath(..)
        , Module
        , ModuleName(..)
        , ModuleType(..)
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any
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
    PP.Config ParseContext ParseProblem Frontend.Expr


module_ : FilePath -> Parser_ (Module Frontend.Expr)
module_ filePath =
    P.succeed
        (\( moduleType_, moduleName_, exposing_ ) dependencies_ topLevelDeclarations_ ->
            { dependencies = dependencies_
            , name = moduleName_
            , filePath = filePath
            , topLevelDeclarations =
                topLevelDeclarations_
                    |> List.map
                        (\almostDeclaration ->
                            let
                                declaration =
                                    almostDeclaration moduleName_
                            in
                            ( declaration.name, declaration )
                        )
                    |> Dict.Any.fromList Common.varNameToString
            , type_ = moduleType_
            , exposing_ = exposing_
            }
        )
        |= moduleDeclaration
        -- TODO what about module doc comment? is it before the imports or after?
        |= dependencies
        |= topLevelDeclarations


moduleDeclaration : Parser_ ( ModuleType, ModuleName, Exposing )
moduleDeclaration =
    P.succeed
        (\moduleType_ moduleName_ exposing_ ->
            ( moduleType_
            , ModuleName moduleName_
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


dependencies : Parser_ (Dict_ ModuleName Dependency)
dependencies =
    P.succeed
        (List.map (\dep -> ( dep.moduleName, dep ))
            >> Dict.Any.fromList Common.moduleNameToString
        )
        |= manyWith P.spaces dependency


dependency : Parser_ Dependency
dependency =
    P.succeed
        (\moduleName_ as_ exposing_ ->
            { moduleName = ModuleName moduleName_
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
            [ P.succeed (ModuleName >> Just)
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
            (\list ->
                if List.isEmpty list then
                    P.problem ExpectingModuleName

                else
                    P.succeed (String.join "." list)
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
            (\list ->
                if List.isEmpty list then
                    P.problem ExposingListCantBeEmpty

                else
                    P.succeed (ExposingSome list)
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


topLevelDeclarations : Parser_ (List (ModuleName -> TopLevelDeclaration Frontend.Expr))
topLevelDeclarations =
    manyWith P.spaces
        (P.succeed identity
            |= topLevelDeclaration
            |. P.spaces
        )


topLevelDeclaration : Parser_ (ModuleName -> TopLevelDeclaration Frontend.Expr)
topLevelDeclaration =
    P.succeed
        (\name body module__ ->
            { module_ = module__
            , name = name
            , body = body
            }
        )
        |= P.map VarName varName
        |. P.spaces
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |. P.spaces
        |= expr


expr : Parser_ Frontend.Expr
expr =
    PP.expression
        { oneOf =
            [ if_
            , let_
            , lambda
            , PP.literal literal
            , always var
            , unit
            , parenthesizedExpr
            ]
        , andThenOneOf =
            -- TODO test this: does `x =\n  call 1\n+ something` work? (it shouldn't: no space before '+')
            [ PP.infixLeft 99
                checkNotBeginningOfLine
                (\fn argument ->
                    Frontend.Call
                        { fn = fn
                        , argument = argument
                        }
                )
            , PP.infixLeft 1 (P.symbol (P.Token "+" ExpectingPlusOperator)) Plus
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


parenthesizedExpr : ExprConfig -> Parser_ Frontend.Expr
parenthesizedExpr config =
    P.succeed identity
        |. P.symbol (P.Token "(" ExpectingLeftParen)
        |= PP.subExpression 0 config
        |. P.symbol (P.Token ")" ExpectingRightParen)


literal : Parser_ Frontend.Expr
literal =
    P.succeed Literal
        |= P.oneOf
            -- TODO literalFloat
            [ literalInt
            , literalChar
            , literalString
            , literalBool
            ]
        |> P.inContext InLiteral


literalInt : Parser_ Literal
literalInt =
    let
        int =
            {- This deals with the bug [elm/parser#25](https://github.com/elm/parser/issues/25).
               In our case it was interfering with `if ... then ... >e<lse ...`.

               For more info see @dmy's assessment at [dmy/elm-pratt-parser#1](https://github.com/dmy/elm-pratt-parser/issues/1).
            -}
            P.backtrackable <|
                P.int ExpectingInt InvalidInt

        hexInt =
            P.succeed identity
                |. P.symbol (P.Token "0x" ExpectingHexPrefix)
                |= P.getChompedString (P.chompWhile Char.isHexDigit)
                |> P.andThen
                    (\hexString ->
                        Hex.fromString hexString
                            |> Result.map P.succeed
                            |> Result.withDefault (P.problem InvalidHexInt)
                    )
                |> P.inContext InHexInt
    in
    P.succeed Int
        |= P.oneOf
            [ P.succeed negate
                |. P.symbol (P.Token "-" ExpectingMinusSign)
                |= P.oneOf
                    [ int
                    , hexInt
                    ]
            , int
            , hexInt
            ]
        |> P.inContext InLiteralInt


{-| TODO escapes
TODO Unicode escapes
-}
literalChar : Parser_ Literal
literalChar =
    (P.succeed identity
        |. P.symbol (P.Token "'" ExpectingSingleQuote)
        |= P.getChompedString (P.chompIf (always True) ExpectingChar)
        |. P.symbol (P.Token "'" ExpectingSingleQuote)
    )
        |> P.andThen
            (\string ->
                string
                    |> String.uncons
                    |> Maybe.map (Tuple.first >> Char >> P.succeed)
                    |> Maybe.withDefault (P.problem (CompilerBug "Multiple characters chomped in `literalChar`"))
            )


{-| TODO escapes
TODO unicode escapes
TODO triple-quoted strings with different escaping
-}
literalString : Parser_ Literal
literalString =
    let
        doubleQuote =
            P.Token "\"" ExpectingDoubleQuote
    in
    P.succeed String
        |. P.symbol doubleQuote
        |= P.getChompedString (P.chompUntil doubleQuote)
        |. P.symbol doubleQuote


literalBool : Parser_ Literal
literalBool =
    P.succeed Bool
        |= P.oneOf
            [ P.map (always True) <| P.keyword (P.Token "True" ExpectingTrue)
            , P.map (always False) <| P.keyword (P.Token "False" ExpectingTrue)
            ]


var : Parser_ Frontend.Expr
var =
    P.oneOf
        [ P.map
            (\varName_ -> Frontend.var Nothing (VarName varName_))
            varName
        , qualifiedVar
        ]


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
        , separator = P.Token "." ExpectingModuleDot
        , end = P.Token "" (CompilerBug "qualifiedVar end parser failed") -- TODO is this the right way?
        , spaces = P.succeed ()
        , item = moduleNameWithoutDots
        , trailing = P.Mandatory -- this is the difference from `moduleName`
        }
        |> P.andThen
            (\list ->
                let
                    maybeModuleName =
                        if List.isEmpty list then
                            Nothing

                        else
                            Just (ModuleName (String.join "." list))
                in
                P.map
                    (\varName_ -> Frontend.var maybeModuleName (VarName varName_))
                    varName
            )


lambda : ExprConfig -> Parser_ Frontend.Expr
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
                (Frontend.transform (promoteArguments arguments) body)
        )
        |. P.symbol (P.Token "\\" ExpectingBackslash)
        |= manyWith spacesOnly (P.map VarName varName)
        |. spacesOnly
        |. P.symbol (P.Token "->" ExpectingRightArrow)
        |. P.spaces
        |= PP.subExpression 0 config


if_ : ExprConfig -> Parser_ Frontend.Expr
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


let_ : ExprConfig -> Parser_ Frontend.Expr
let_ config =
    P.succeed
        (\binding_ body ->
            Frontend.Let
                -- TODO multiple let bindings
                { bindings = [ binding_ ]
                , body = body
                }
        )
        |. P.keyword (P.Token "let" ExpectingIf)
        |. P.spaces
        |= binding config
        |. P.spaces
        |. P.keyword (P.Token "in" ExpectingThen)
        |. P.spaces
        |= PP.subExpression 0 config
        |> P.inContext InLet


binding : ExprConfig -> Parser_ (Binding Frontend.Expr)
binding config =
    P.succeed Binding
        |= P.map VarName varName
        |. P.spaces
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |. P.spaces
        |= PP.subExpression 0 config
        |> P.inContext InLetBinding


promoteArguments : List VarName -> Frontend.Expr -> Frontend.Expr
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


unit : ExprConfig -> Parser_ Frontend.Expr
unit _ =
    P.succeed Frontend.Unit
        |. P.keyword (P.Token "()" ExpectingUnit)
        |> P.inContext InUnit



-- Helpers


spacesOnly : Parser_ ()
spacesOnly =
    P.chompWhile ((==) ' ')


newlines : Parser_ ()
newlines =
    P.chompWhile ((==) '\n')


newlinesAndOptionallySpaces : Parser_ ()
newlinesAndOptionallySpaces =
    P.succeed ()
        |. P.spaces
        |. newlines
        |. P.spaces


{-| Taken from Punie/elm-parser-extras, made to work with Parser.Advanced.Parser
instead of the simple one.
-}
manyWith : Parser_ () -> Parser_ a -> Parser_ (List a)
manyWith spaces p =
    P.loop [] (manyHelp spaces p)


{-| Taken from Punie/elm-parser-extras, made to work with Parser.Advanced.Parser
instead of the simple one.
-}
manyHelp : Parser_ () -> Parser_ a -> List a -> Parser_ (P.Step (List a) (List a))
manyHelp spaces p vs =
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
