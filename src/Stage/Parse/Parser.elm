module Stage.Parse.Parser exposing
    ( dependencies
    , exposingList
    , moduleDeclaration
    , moduleName
    , module_
    )

import AST.Common exposing (Literal(..))
import AST.Frontend as Frontend exposing (Expr(..))
import Common
import Common.Types
    exposing
        ( Dependency
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
import Parser.Advanced as P exposing ((|.), (|=), Parser)
import Set exposing (Set)


type alias Parser_ a =
    Parser ParseContext ParseProblem a


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
        |= many dependency


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
        { start = P.Token "" ShouldntHappen -- TODO is this the right way?
        , separator = P.Token "." ExpectingModuleDot
        , end = P.Token "" ShouldntHappen
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
        |> P.inContext InModuleName


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
    many
        (P.succeed identity
            |= topLevelDeclaration
            |. P.spaces
        )


topLevelDeclaration : Parser_ (ModuleName -> TopLevelDeclaration Frontend.Expr)
topLevelDeclaration =
    P.succeed TopLevelDeclaration
        |= P.map VarName varName
        |. P.spaces
        |. P.symbol (P.Token "=" ExpectingEqualsSign)
        |. P.spaces
        |= expr


{-| TODO absolutely revamp this wrt. operators ... use <https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/>
Ditch the terminal / non-terminal stuff.

TODO parse lambdas!

-}
expr : Parser_ Frontend.Expr
expr =
    P.oneOf
        [ nonTerminal
        , terminal
        ]


terminal : Parser_ Frontend.Expr
terminal =
    P.oneOf
        [ literal
        , var
        ]


nonTerminal : Parser_ Frontend.Expr
nonTerminal =
    P.oneOf
        [ plus
        ]


literal : Parser_ Frontend.Expr
literal =
    P.succeed Literal
        |= P.oneOf
            [ literalInt
            ]


literalInt : Parser_ Literal
literalInt =
    let
        int =
            P.int ExpectingInt InvalidInt
    in
    P.succeed LInt
        |= P.oneOf
            [ P.succeed negate
                |. P.symbol (P.Token "-" ExpectingMinusSign)
                |= int
            , int
            ]


var : Parser_ Frontend.Expr
var =
    P.oneOf
        [ P.map (\v -> Var ( Nothing, VarName v )) varName
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
        { start = P.Token "" ShouldntHappen -- TODO is this the right way?
        , separator = P.Token "." ExpectingModuleDot
        , end = P.Token "" ShouldntHappen
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
                P.map (\varName_ -> Var ( maybeModuleName, VarName varName_ )) varName
            )
        |> P.inContext InQualifiedVar


plus : Parser_ Frontend.Expr
plus =
    P.lazy (\() -> terminal |> P.andThen (plusHelper []))


plusHelper : List Expr -> Expr -> Parser_ Frontend.Expr
plusHelper revExprsSoFar currentExpr =
    P.oneOf
        [ P.succeed identity
            |. spacesOnly
            |. P.symbol (P.Token "+" ExpectingPlusOperator)
            |. spacesOnly
            |= terminal
            |> P.andThen (\newExpr -> plusHelper (currentExpr :: revExprsSoFar) newExpr)
        , P.lazy (\() -> P.succeed (List.foldl Plus currentExpr revExprsSoFar))
        ]



-- Helpers


spacesOnly : Parser_ ()
spacesOnly =
    P.chompWhile ((==) ' ')


newlines : Parser_ ()
newlines =
    P.chompWhile ((==) '\n')


newlineOrEnd : Parser_ ()
newlineOrEnd =
    P.oneOf
        [ P.symbol (P.Token "\n" ExpectingNewline)
        , P.end ExpectingEnd
        , P.succeed ()
        ]


{-| Taken from Punie/elm-parser-extras, made to work with Parser.Advanced.Parser
instead of the simple one.
-}
many : Parser_ a -> Parser_ (List a)
many p =
    P.loop [] (manyHelp p)


{-| Taken from Punie/elm-parser-extras, made to work with Parser.Advanced.Parser
instead of the simple one.
-}
manyHelp : Parser_ a -> List a -> Parser_ (P.Step (List a) (List a))
manyHelp p vs =
    P.oneOf
        [ P.succeed (\v -> P.Loop (v :: vs))
            |= p
            |. P.spaces
        , P.succeed ()
            |> P.map (always (P.Done (List.reverse vs)))
        ]
