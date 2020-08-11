module Elm.Data.Binding exposing
    ( Binding, combine, map
    , Commented, fromCommented, mapCommented
    )

{-| Binding in the `let...in` expression.

    let
        myNumber =
            123

        answer =
            42
    in
    myNumber + answer

contains two bindings: `myNumber` and `answer`.

@docs Binding, combine, map
@docs Commented, fromCommented, mapCommented

-}

import Elm.Data.Comment exposing (Comment)


{-| -}
type alias Binding expr =
    -- TODO type annotation for the let...in binding
    { name : String
    , body : expr
    }


{-| Apply a function to the expression inside the binding.
-}
map : (e1 -> e2) -> Binding e1 -> Binding e2
map fn { name, body } =
    { name = name
    , body = fn body
    }


{-| Switch the Result and the expression inside the binding.
Similar to [`Result.Extra.combine`](/packages/elm-community/result-extra/latest/Result-Extra#combine).

    combine { name = "foo", body = Ok (Int 5) }
    --> Ok { name = "foo", body = Int 5 }

-}
combine : Binding (Result x a) -> Result x (Binding a)
combine { name, body } =
    Result.map
        (\body_ ->
            { name = name
            , body = body_
            }
        )
        body


{-| Binding with comments:

        x {- commentsAfterName -} = {- commentsBeforeBody -} 2

-}
type alias Commented expr =
    -- TODO type annotation for the let...in binding
    { name : String
    , commentsAfterName : List Comment
    , commentsBeforeBody : List Comment
    , body : expr
    }


{-| Create a [Binding](#Binding) from a [Commented](#Commented).
-}
fromCommented : Commented e -> Binding e
fromCommented { name, body } =
    { name = name
    , body = body
    }


{-| Apply a function to the expression inside the commented binding.
-}
mapCommented : (e1 -> e2) -> Commented e1 -> Commented e2
mapCommented fn { name, commentsAfterName, commentsBeforeBody, body } =
    { name = name
    , commentsAfterName = commentsAfterName
    , commentsBeforeBody = commentsBeforeBody
    , body = fn body
    }
