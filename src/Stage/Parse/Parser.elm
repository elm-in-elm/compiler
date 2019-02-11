module Stage.Parse.Parser exposing (module_)

import AST.Common as Common
import AST.Frontend as Frontend
import Common
import Common.Types
    exposing
        ( ExposedItem(..)
        , Exposing(..)
        , FilePath(..)
        , Module
        , ModuleName(..)
        , ModuleType(..)
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
import Set.Any


type alias Parser_ a =
    Parser ParseContext ParseProblem a


module_ : FilePath -> Parser_ (Module Frontend.Expr)
module_ filePath =
    P.oneOf
        [ plainModule filePath
        , portModule filePath
        , effectModule filePath
        ]


plainModule : FilePath -> Parser_ (Module Frontend.Expr)
plainModule filePath =
    generalModule PlainModule filePath


{-| Example: port module Foo exposing (bar)
-}
portModule : FilePath -> Parser_ (Module Frontend.Expr)
portModule filePath =
    P.succeed identity
        |. P.keyword (P.Token "port" ExpectingPortKeyword)
        |. spacesOnly
        -- TODO or is there a newline allowed between the words `port` and `module`?
        |= generalModule PortModule filePath


{-| TODO
-}
effectModule : FilePath -> Parser_ (Module Frontend.Expr)
effectModule filePath =
    P.problem TodoNotImplemented


generalModule : ModuleType -> FilePath -> Parser_ (Module Frontend.Expr)
generalModule moduleType filePath =
    P.succeed
        (\( moduleName_, exposing_ ) ->
            { dependencies = Set.Any.empty Common.moduleNameToString -- TODO
            , name = moduleName_
            , filePath = filePath
            , topLevelDeclarations = Dict.Any.empty Common.varNameToString -- TODO
            , type_ = moduleType
            , exposing_ = exposing_
            }
        )
        |= moduleDeclaration moduleType


moduleDeclaration : ModuleType -> Parser_ ( ModuleName, Exposing )
moduleDeclaration moduleType =
    P.succeed
        (\moduleName_ exposing_ ->
            ( ModuleName moduleName_
            , exposing_
            )
        )
        |. P.keyword (P.Token "module" ExpectingModuleKeyword)
        |. spacesOnly
        -- TODO check the assumption... does Elm allow newlines there?
        |= moduleName
        |. P.spaces
        |. P.keyword (P.Token "exposing" ExpectingExposingKeyword)
        |. P.spaces
        |= exposingList


moduleName : Parser_ String
moduleName =
    P.variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '.'
        , reserved = Set.empty
        , expecting = ExpectingModuleName
        }


exposingList : Parser_ Exposing
exposingList =
    P.oneOf
        [ exposingAll
        , exposingExplicit
        ]


exposingAll : Parser_ Exposing
exposingAll =
    P.symbol (P.Token "(..)" ExpectingExposingAllSymbol)
        |> P.map (always ExposingAll)


exposingExplicit : Parser_ Exposing
exposingExplicit =
    P.sequence
        { start = P.Token "(" ExpectingExposingListLeftParen
        , separator = P.Token "," ExpectingExposingListSeparatorComma
        , end = P.Token ")" ExpectingExposingListRightParen
        , spaces = P.spaces
        , item = exposedItem
        , trailing = P.Forbidden
        }
        |> P.map ExposingSome


exposedItem : Parser_ ExposedItem
exposedItem =
    P.oneOf
        [ exposedValue

        -- those two have to be in this order:
        , exposedTypeAndAllConstructors
        , exposedType
        ]


exposedValue : Parser_ ExposedItem
exposedValue =
    P.map ExposedValue varName


exposedType : Parser_ ExposedItem
exposedType =
    P.map ExposedType typeOrConstructorName


exposedTypeAndAllConstructors : Parser_ ExposedItem
exposedTypeAndAllConstructors =
    P.succeed identity
        |= P.map ExposedTypeAndAllConstructors typeOrConstructorName
        |. P.symbol (P.Token "(..)" ExpectingExposedTypeDoublePeriod)


varName : Parser_ String
varName =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedWords
        , expecting = ExpectingVarName
        }


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


spacesOnly : Parser_ ()
spacesOnly =
    P.chompWhile ((==) ' ')
