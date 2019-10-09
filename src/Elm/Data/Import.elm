module Elm.Data.Import exposing (Import)

{-| The import statement.

      import Foo
      --> Import "Foo Nothing Nothing

      import Foo.Bar
      --> Import "Foo.Bar" Nothing Nothing

      import Foo as F
      --> Import "Foo" (Just "F") Nothing

      import Foo exposing (..)
      --> Import "Foo" Nothing (Just ExposingAll)

      import Foo as F exposing (..)
      --> Import "Foo" (Just "F") (Just ExposingAll)

@docs Import

-}

import Elm.Data.Exposing exposing (Exposing)
import Elm.Data.ModuleName exposing (ModuleName)


{-| -}
type alias Import =
    { moduleName : ModuleName
    , as_ : Maybe ModuleName
    , exposing_ : Maybe Exposing
    }
