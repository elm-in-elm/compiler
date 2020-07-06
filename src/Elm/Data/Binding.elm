module Elm.Data.Binding exposing (Binding, combine, map)

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

-}

import Elm.Data.VarName exposing (VarName)


{-| -}
type alias Binding expr =
    -- TODO type annotation for the let...in binding
    { name : VarName
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
