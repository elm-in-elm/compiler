module Elm.Data.Exposing exposing (Exposing(..), ExposedItem(..), name)

{-| The `exposing (...)` line of module header and import statements.

    module Foo exposing (..)
    --> ExposingAll

    module Foo exposing (bar)
    --> ExposingSome [ExposedValue "bar"]

    module Foo exposing (Bar)
    --> ExposingSome [ExposedType "Bar"]

    module Foo exposing (Bar(..))
    --> ExposingSome [ExposedTypeAndAllConstructors "Bar"]

@docs Exposing, ExposedItem, name

-}

import Elm.Data.Comment exposing (Comment)
import Elm.Data.VarName exposing (VarName)


{-| -}
type Exposing
    = ExposingAll -- exposing (..)
    | ExposingSome
        (List
            { commentsBefore : List Comment -- exposing (>{-before-}< foo)
            , item : ExposedItem -- exposing (>foo<, Foo, Bar(..))
            , commentsAfter : List Comment -- exposing (foo >{-after-}<)
            }
        )


{-| -}
type ExposedItem
    = ExposedValue VarName -- exposing (foo)
    | ExposedType VarName -- exposing (Foo)
    | ExposedTypeAndAllConstructors VarName -- exposing (Foo(..))


{-| Unwraps the variable or type name from the [`ExposedItem`](#ExposedItem).

    name (ExposedValue "foo")
    --> "foo"

    name (ExposedType "Foo")
    --> "Foo"

    name (ExposedTypeAndAllConstructors "Foo")
    --> "Foo"

-}
name : ExposedItem -> VarName
name item =
    case item of
        ExposedValue name_ ->
            name_

        ExposedType name_ ->
            name_

        ExposedTypeAndAllConstructors name_ ->
            name_
