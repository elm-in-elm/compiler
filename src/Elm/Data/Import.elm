module Elm.Data.Import exposing (Import)

import Elm.Data.Exposing exposing (Exposing)


{-| All four possibilities of the Maybes make sense:

                   | exposing_ = Nothing | exposing_ = Just ...
    ---------------|---------------------|-------------------------------
    as_ = Nothing  | import Foo          | import Foo exposing (..)
    as_ = Just "F" | import Foo as F     | import Foo as F exposing (..)

-}
type alias Import =
    { moduleName : String
    , as_ : Maybe String
    , exposing_ : Maybe Exposing
    }
