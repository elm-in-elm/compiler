module Elm.Data.TypeAnnotation exposing (TypeAnnotation)

import Elm.Data.Type exposing (Type)
import Elm.Data.VarName exposing (VarName)


{-| Represents a type annotation like:

    x : Int

-}
type alias TypeAnnotation =
    { varName : VarName
    , type_ : Type
    }
