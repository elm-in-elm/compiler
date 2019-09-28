module Elm.Data.Exposing exposing
    ( ExposedItem(..)
    , Exposing(..)
    , name
    )


type Exposing
    = ExposingAll -- exposing (..)
    | ExposingSome (List ExposedItem) -- exposing (foo, Foo, Bar(..))


type ExposedItem
    = ExposedValue String -- exposing (foo)
    | ExposedType String -- exposing (Foo)
    | ExposedTypeAndAllConstructors String -- exposing (Foo(..))


name : ExposedItem -> String
name item =
    case item of
        ExposedValue name_ ->
            name_

        ExposedType name_ ->
            name_

        ExposedTypeAndAllConstructors name_ ->
            name_
