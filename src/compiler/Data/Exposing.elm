module Data.Exposing exposing
    ( ExposedItem(..)
    , Exposing(..)
    , name
    )

import Data.VarName exposing (VarName)


type Exposing
    = ExposingAll -- exposing (..)
    | ExposingSome (List ExposedItem) -- exposing (foo, Foo, Bar(..))


type ExposedItem
    = ExposedValue VarName -- exposing (foo)
    | ExposedType VarName -- exposing (Foo)
    | ExposedTypeAndAllConstructors VarName -- exposing (Foo(..))


name : ExposedItem -> VarName
name item =
    case item of
        ExposedValue name_ ->
            name_

        ExposedType name_ ->
            name_

        ExposedTypeAndAllConstructors name_ ->
            name_
