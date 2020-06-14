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
    { varName :
        VarName
            -- TODO start here:
            -- I was implementing stuff around `desugarType`
            -- it dawned on me that this might be easier if
            -- I had the guarantee that the type I'm looking at
            -- is always a Type, never an Id. (See TypeOrId.)
            -- So I changed this type_ : Type PQ
            -- into type_ : ConcreteType PQ.
            -- Now go and see what needs to be changed to propagate
            -- these changes to `desugarType` etc!
            start
            here
    , type_ : ConcreteType PossiblyQualified
    }
