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
import Elm.Data.VarName exposing (VarName)
import Result.Extra
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing ({- TODO maybe move SubstMap module to Elm.Data? -} SubstitutionMap)


{-| -}
type alias Declaration expr annotation qualifiedness =
    { module_ : ModuleName

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
    , name : VarName
    , body : DeclarationBody expr qualifiedness
    }


{-|

     x = 1
     --> Value (Int 1)

     type alias X = Int
     --> TypeAlias [] Int

     type alias X a = Maybe a
     --> TypeAlias ["a"] (Maybe (Var 0))

-}
type DeclarationBody expr qualifiedness
    = Value expr
    | TypeAlias
        { parameters : List VarName -- on the left side of =
        , -- TODO how to map from the parameters to the vars in the definition?
          definition : Type qualifiedness
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
type alias Constructor a =
    { name : String
    , arguments : List (TypeOrId a)
    }


{-| Apply a function to the expression inside the declaration.
-}
map :
    (exprA -> exprB)
    -> (qualifiednessA -> qualifiednessB)
    -> Declaration exprA annotation qualifiednessA
    -> Declaration exprB annotation qualifiednessB
map fnExpr fnQualifiedness declaration =
    { module_ = declaration.module_
    , typeAnnotation = declaration.typeAnnotation
    , name = declaration.name
    , body = mapBody fnExpr fnQualifiedness declaration.body
    }


{-| Replace the annotation with another of possibly different type.
-}
setAnnotation :
    Maybe ann2
    -> Declaration a ann1 b
    -> Declaration a ann2 b
setAnnotation annotation declaration =
    { module_ = declaration.module_
    , typeAnnotation = annotation
    , name = declaration.name
    , body = declaration.body
    }


{-| Apply a function to the expression inside the declaration body.
-}
mapBody :
    (exprA -> exprB)
    -> (qualifiednessA -> qualifiednessB)
    -> DeclarationBody exprA qualifiednessA
    -> DeclarationBody exprB qualifiednessB
mapBody fnExpr fnQualifiedness body =
    case body of
        Value expr ->
            Value <| fnExpr expr

        TypeAlias r ->
            TypeAlias
                { parameters = r.parameters
                , definition = Type.mapType fnQualifiedness r.definition
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
combineValue : DeclarationBody (Result err a) b -> Result err (DeclarationBody a b)
combineValue body =
    case body of
        Value result ->
            Result.map Value result

        TypeAlias r ->
            Ok <| TypeAlias r

        CustomType r ->
            Ok <| CustomType r


combineType : DeclarationBody a (Result err b) -> Result err (DeclarationBody a b)
combineType body =
    case body of
        Value expr ->
            Ok <| Value expr

        TypeAlias r ->
            r.definition
                |> Type.combineType
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
    DeclarationBody ( expr, SubstitutionMap ) a
    -> ( DeclarationBody expr a, SubstitutionMap )
combineSubstitutionMap body =
    {- TODO very unsure about this. Are we ever merging those empty
       SubstitutionMaps with the non-empty ones?
    -}
    case body of
        Value ( expr, map_ ) ->
            ( Value expr, map_ )

        TypeAlias r ->
            ( TypeAlias r, SubstitutionMap.empty )

        CustomType r ->
            ( CustomType r, SubstitutionMap.empty )


getExpr : Declaration expr a b -> Maybe expr
getExpr decl =
    case decl.body of
        Value expr ->
            Just expr

        _ ->
            Nothing


mapConstructor : (a -> b) -> Constructor a -> Constructor b
mapConstructor fn constructor =
    { name = constructor.name
    , arguments = List.map (Type.mapTypeOrId fn) constructor.arguments
    }


combineConstructor : Constructor (Result err a) -> Result err (Constructor a)
combineConstructor constructor =
    constructor.arguments
        |> List.map Type.combineTypeOrId
        |> Result.Extra.combine
        |> Result.map
            (\arguments ->
                { name = constructor.name
                , arguments = arguments
                }
            )
