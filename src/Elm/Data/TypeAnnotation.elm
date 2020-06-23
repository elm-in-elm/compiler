module Elm.Data.TypeAnnotation exposing (TypeAnnotation)

import Elm.Data.Qualifiedness exposing (PossiblyQualified)
import Elm.Data.Type.Concrete exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)


{-| Represents a type annotation like:

    x : Int

The user type modules can be unqualified in type annotations:

     import Page exposing (Msg)
     x : Msg -- unqualified; `PossiblyQualified Nothing`

but also qualified:

     import Page
     x : Page.Msg -- qualified; `PossiblyQualified (Just "Page")`

-}
type alias TypeAnnotation =
    { varName : VarName
    , type_ : ConcreteType PossiblyQualified
    }
