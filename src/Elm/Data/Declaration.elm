module Elm.Data.Declaration exposing
    ( Declaration, DeclarationBody(..), Constructor
    , map, mapBody, setAnnotation
    , combineValue, combineType, combineSubstitutionMap
    , getExpr
    )

{-| Top-level declaration, be it a function, constant or a type definition.

@docs Declaration, DeclarationBody, Constructor
@docs map, mapBody, setAnnotation
@docs combineValue, combineType, combineSubstitutionMap
@docs getExpr

-}

import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type as Type exposing (Type, TypeOrId)
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import Result.Extra
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing ({- TODO maybe move SubstMap module to Elm.Data? -} SubstitutionMap)


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
    | TypeAlias
        { parameters : List VarName -- on the left side of =
        , -- TODO how to map from the parameters to the vars in the definition?
          definition : ConcreteType qualifiedness
        }
    | CustomType
        { parameters : List VarName -- on the left side of =
        , constructors : List (Constructor qualifiedness)
        }


{-| Constructor of a custom type.

     type Foo = Bar
     --> CustomType [] [Constructor "Bar" []]

     type Foo a = Bar
     --> CustomType [] [Constructor "Bar" []]

     type Foo a = Bar a
     --> CustomType ["a"] [Constructor "Bar" ["a"]]

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
                , constructors = List.map (mapConstructor fnQualifiedness) r.constructors
                }


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
                |> List.map combineConstructor
                |> Result.Extra.combine
                |> Result.map
                    (\constructors ->
                        CustomType
                            { parameters = r.parameters
                            , constructors = constructors
                            }
                    )


combineSubstitutionMap :
    DeclarationBody ( expr, SubstitutionMap ) a b
    -> ( DeclarationBody expr a b, SubstitutionMap )
combineSubstitutionMap body =
    {- TODO very unsure about this. Are we ever merging those empty
       SubstitutionMaps with the non-empty ones?
    -}
    case body of
        Value r ->
            case r.expression of
                ( expr, map_ ) ->
                    ( Value
                        { expression = expr
                        , typeAnnotation = r.typeAnnotation
                        }
                    , map_
                    )

        TypeAlias r ->
            ( TypeAlias r, SubstitutionMap.empty )

        CustomType r ->
            ( CustomType r, SubstitutionMap.empty )


getExpr : Declaration expr a b -> Maybe expr
getExpr decl =
    case decl.body of
        Value { expression } ->
            Just expression

        _ ->
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
