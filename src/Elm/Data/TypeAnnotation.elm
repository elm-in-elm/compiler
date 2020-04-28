module Elm.Data.TypeAnnotation exposing (TypeAnnotation)

import Elm.Data.Type exposing (Type)
import Elm.Data.VarName exposing (VarName)


{-| Represents a type annotation like:

    x : Int

The user type modules can be unqualified in type annotations:

     import Page exposing (Msg)
     x : Msg -- unqualified

-}
type alias TypeAnnotation =
    { varName : VarName
    , type_ : Type (Maybe String)
    }
