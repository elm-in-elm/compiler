module Elm.Data.Declaration exposing
    ( Declaration, DeclarationBody(..), Constructor, TypeAliasDeclaration
    , map, mapBody, setAnnotation
    , combineValue, combineType, combineTuple3
    , getExpr, getTypeAlias
    )

{-| Top-level declaration, be it a function, constant or a type definition.

@docs Declaration, DeclarationBody, Constructor, TypeAliasDeclaration
@docs map, mapBody, setAnnotation
@docs combineValue, combineType, combineTuple3
@docs getExpr, getTypeAlias

-}

import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import List.NonEmpty exposing (NonEmpty)
import OurExtras.List.NonEmpty
import Result.Extra


{-| -}
type alias Declaration expr annotation qualifiedness =
    { module_ : ModuleName
    , name : VarName
    , body : DeclarationBody expr annotation qualifiedness
    }


{-|

     x = 1
     --> Value (Int 1)

     type alias X = Int
     --> TypeAlias [] Int

     type alias X a = Maybe a
     --> TypeAlias ["a"] (Maybe (Var 0))

-}
type DeclarationBody expr annotation qualifiedness
    = Value
        { expression : expr

        -- What information from the annotation is yet to be used in the current stage?
        -----------------------------------
        -- Nothing: no annotation was given
        -- Just Never: annotation was given but we successfully used all of it
        -----------------------------------
        -- The `annotation` types used are:
        -- FRONTEND: TypeAnnotation (for which we need to check that the name in the
        --                           annotation is the same as the name in the declaration)
        -- CANONICAL: Type (for which we need to check that this advertised type is
        --                  unifiable with the type of the declaration)
        -- TYPED: Never (where we've used up all the info from the annotation and
        --               don't need it anymore)
        , typeAnnotation : Maybe annotation
        }
    | TypeAlias (TypeAliasDeclaration qualifiedness)
    | CustomType
        { parameters : List VarName -- on the left side of =
        , constructors : NonEmpty (Constructor qualifiedness)
        }
    | Port (ConcreteType qualifiedness)


type alias TypeAliasDeclaration qualifiedness =
    { parameters : List VarName -- on the left side of =
    , -- TODO how to map from the parameters to the vars in the definition?
      definition : ConcreteType qualifiedness
    }


{-| Constructor of a custom type.

    type Foo = Bar
    --> CustomType [] [ Constructor "Bar" [] ]

    type Foo a = Bar
    --> CustomType [] [ Constructor "Bar" [] ]

    type Foo a = Bar a
    --> CustomType [ "a" ] [ Constructor "Bar" [ "a" ] ]

    type Foo = Bar | Baz
    --> CustomType []
            [ Constructor "Bar" []
            , Constructor "Baz" []
            ]

-}
type alias Constructor qualifiedness =
    { name : String
    , arguments : List (ConcreteType qualifiedness)
    }


{-| Apply a function to the expression inside the declaration.
-}
map :
    (exprA -> exprB)
    -> (Maybe annotationA -> Maybe annotationB)
    -> (qualifiednessA -> qualifiednessB)
    -> Declaration exprA annotationA qualifiednessA
    -> Declaration exprB annotationB qualifiednessB
map fnExpr fnAnnotation fnQualifiedness declaration =
    { module_ = declaration.module_
    , name = declaration.name
    , body = mapBody fnExpr fnAnnotation fnQualifiedness declaration.body
    }


{-| Replace the annotation with another of possibly different type.
-}
setAnnotation :
    Maybe ann2
    -> Declaration a ann1 b
    -> Declaration a ann2 b
setAnnotation annotation declaration =
    { module_ = declaration.module_
    , name = declaration.name
    , body =
        declaration.body
            |> mapBody
                identity
                (always annotation)
                identity
    }


{-| Apply a function to the expression inside the declaration body.
-}
mapBody :
    (exprA -> exprB)
    -> (Maybe annotationA -> Maybe annotationB)
    -> (qualifiednessA -> qualifiednessB)
    -> DeclarationBody exprA annotationA qualifiednessA
    -> DeclarationBody exprB annotationB qualifiednessB
mapBody fnExpr fnAnnotation fnQualifiedness body =
    case body of
        Value { expression, typeAnnotation } ->
            Value
                { expression = fnExpr expression
                , typeAnnotation = fnAnnotation typeAnnotation
                }

        TypeAlias r ->
            TypeAlias
                { parameters = r.parameters
                , definition = ConcreteType.map fnQualifiedness r.definition
                }

        CustomType r ->
            CustomType
                { parameters = r.parameters
                , constructors = List.NonEmpty.map (mapConstructor fnQualifiedness) r.constructors
                }

        Port type_ ->
            Port (ConcreteType.map fnQualifiedness type_)


{-| Switch the Result and the expression inside the declaration body.
Similar to [`Result.Extra.combine`](/packages/elm-community/result-extra/latest/Result-Extra#combine).

    combineValue (Value (Ok (Int 5)))
    --> Ok (Value (Int 5))

-}
combineValue : DeclarationBody (Result err a) b c -> Result err (DeclarationBody a b c)
combineValue body =
    case body of
        Value r ->
            r.expression
                |> Result.map
                    (\expr ->
                        Value
                            { expression = expr
                            , typeAnnotation = r.typeAnnotation
                            }
                    )

        TypeAlias r ->
            Ok <| TypeAlias r

        CustomType r ->
            Ok <| CustomType r

        Port type_ ->
            Ok <| Port type_


combineType : DeclarationBody a b (Result err c) -> Result err (DeclarationBody a b c)
combineType body =
    case body of
        Value expr ->
            Ok <| Value expr

        TypeAlias r ->
            r.definition
                |> ConcreteType.combine
                |> Result.map
                    (\definition ->
                        TypeAlias
                            { parameters = r.parameters
                            , definition = definition
                            }
                    )

        CustomType r ->
            r.constructors
                |> List.NonEmpty.map combineConstructor
                |> OurExtras.List.NonEmpty.combine
                |> Result.map
                    (\constructors ->
                        CustomType
                            { parameters = r.parameters
                            , constructors = constructors
                            }
                    )

        Port type_ ->
            type_
                |> ConcreteType.combine
                |> Result.map Port


combineTuple3 :
    ( x, y, z )
    -> DeclarationBody ( a, ( x, y, z ) ) b c
    -> ( DeclarationBody a b c, ( x, y, z ) )
combineTuple3 ( defaultX, defaultY, defaultZ ) body =
    case body of
        Value r ->
            case r.expression of
                ( expr, ( x, y, z ) ) ->
                    ( Value
                        { expression = expr
                        , typeAnnotation = r.typeAnnotation
                        }
                    , ( x, y, z )
                    )

        TypeAlias r ->
            ( TypeAlias r, ( defaultX, defaultY, defaultZ ) )

        CustomType r ->
            ( CustomType r, ( defaultX, defaultY, defaultZ ) )

        Port type_ ->
            ( Port type_, ( defaultX, defaultY, defaultZ ) )


getExpr : DeclarationBody expr a b -> Maybe expr
getExpr body =
    case body of
        Value { expression } ->
            Just expression

        TypeAlias _ ->
            Nothing

        CustomType _ ->
            Nothing

        Port _ ->
            Nothing


getTypeAlias : Declaration a b qualifiedness -> Maybe (TypeAliasDeclaration qualifiedness)
getTypeAlias decl =
    case decl.body of
        Value _ ->
            Nothing

        TypeAlias r ->
            Just r

        CustomType _ ->
            Nothing

        Port _ ->
            Nothing


mapConstructor : (a -> b) -> Constructor a -> Constructor b
mapConstructor fn constructor =
    { name = constructor.name
    , arguments = List.map (ConcreteType.map fn) constructor.arguments
    }


combineConstructor : Constructor (Result err a) -> Result err (Constructor a)
combineConstructor constructor =
    constructor.arguments
        |> List.map ConcreteType.combine
        |> Result.Extra.combine
        |> Result.map
            (\arguments ->
                { name = constructor.name
                , arguments = arguments
                }
            )
