module Elm.Data.Declaration exposing
    ( Declaration, DeclarationBody(..), Constructor
    , map, mapBody, combine, combineSubstitutionMap
    , getExpr
    )

{-| Top-level declaration, be it a function, constant or a type definition.

@docs Declaration, DeclarationBody, Constructor
@docs map, mapBody, combine, combineSubstitutionMap
@docs getExpr

-}

import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Type exposing (Type, TypeOrId)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing ({- TODO maybe move SubstMap module to Elm.Data? -} SubstitutionMap)


{-| -}
type alias Declaration expr annotation =
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
    , body : DeclarationBody expr
    }


{-|

     x = 1
     --> Value (Int 1)

     type alias X = Int
     --> TypeAlias [] Int

     type alias X a = Maybe a
     --> TypeAlias ["a"] (Maybe (Var 0))

-}
type DeclarationBody expr
    = Value expr
    | TypeAlias
        { parameters : List VarName -- on the left side of =
        , -- TODO how to map from the parameters to the vars in the definition?
          definition : Type
        }
    | CustomType
        { parameters : List VarName -- on the left side of =
        , constructors : List Constructor
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
type alias Constructor =
    { name : String
    , arguments : List TypeOrId
    }


{-| Apply a function to the expression inside the declaration.
-}
map : (exprA -> exprB) -> Declaration exprA annotation -> Declaration exprB annotation
map fn declaration =
    { module_ = declaration.module_
    , typeAnnotation = declaration.typeAnnotation
    , name = declaration.name
    , body = mapBody fn declaration.body
    }


{-| Apply a function to the expression inside the declaration body.
-}
mapBody : (exprA -> exprB) -> DeclarationBody exprA -> DeclarationBody exprB
mapBody fn body =
    case body of
        Value expr ->
            Value <| fn expr

        TypeAlias r ->
            TypeAlias r

        CustomType r ->
            CustomType r


{-| Switch the Result and the expression inside the declaration body.
Similar to [`Result.Extra.combine`](/packages/elm-community/result-extra/latest/Result-Extra#combine).

    combine (Value (Ok (Int 5)))
    --> Ok (Value (Int 5))

-}
combine : DeclarationBody (Result err a) -> Result err (DeclarationBody a)
combine body =
    case body of
        Value result ->
            Result.map Value result

        TypeAlias r ->
            Ok <| TypeAlias r

        CustomType r ->
            Ok <| CustomType r


combineSubstitutionMap : DeclarationBody ( expr, SubstitutionMap ) -> ( DeclarationBody expr, SubstitutionMap )
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


getExpr : Declaration expr annotation -> Maybe expr
getExpr decl =
    case decl.body of
        Value expr ->
            Just expr

        _ ->
            Nothing
