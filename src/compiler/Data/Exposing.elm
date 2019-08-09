module Data.Exposing exposing
    ( ExposedItem(..)
    , Exposing(..)
    )


type Exposing
    = ExposingAll -- exposing (..)
    | ExposingSome (List ExposedItem) -- exposing (foo, Foo, Bar(..))


type ExposedItem
    = ExposedValue String -- exposing (foo)
    | ExposedType String -- exposing (Foo)
    | ExposedTypeAndAllConstructors String -- exposing (Foo(..))
