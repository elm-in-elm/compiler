module Data.Import exposing (Import)

import Data.Exposing exposing (Exposing)
import Data.ModuleName exposing (ModuleName)


{-| All four possibilities of the Maybes make sense:

                   | exposing_ = Nothing | exposing_ = Just ...
    ---------------|---------------------|-------------------------------
    as_ = Nothing  | import Foo          | import Foo exposing (..)
    as_ = Just "F" | import Foo as F     | import Foo as F exposing (..)

-}
type alias Import =
    { moduleName : ModuleName
    , as_ : Maybe ModuleName
    , exposing_ : Maybe Exposing
    }
