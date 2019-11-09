module Elm.Data.Declaration exposing
    ( Declaration, DeclarationBody(..), Constructor
    , map, mapBody, combine
    )

{-| Top-level declaration, be it a function, constant or a type definition.

@docs Declaration, DeclarationBody, Constructor
@docs map, mapBody, combine

-}

import Elm.Data.Type exposing (Type, TypeArgument)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Elm.Data.VarName exposing (VarName)


{-| -}
type alias Declaration expr =
    { module_ : String
    , typeAnnotation : Maybe TypeAnnotation -- TODO desugar from Maybe TypeAnnotation to Maybe Type
    , name : String
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
    , arguments : List TypeArgument
    }


{-| Apply a function to the expression inside the declaration.
-}
map : (a -> b) -> Declaration a -> Declaration b
map fn declaration =
    { module_ = declaration.module_
    , typeAnnotation = declaration.typeAnnotation
    , name = declaration.name
    , body = mapBody fn declaration.body
    }


{-| Apply a function to the expression inside the declaration body.
-}
mapBody : (a -> b) -> DeclarationBody a -> DeclarationBody b
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
