module Data.Binding exposing
    ( Binding
    , combine
    , map
    )

import Data.VarName exposing (VarName)


type alias Binding expr =
    { name : VarName
    , body : expr
    }


map : (e1 -> e2) -> Binding e1 -> Binding e2
map fn { name, body } =
    { name = name
    , body = fn body
    }


combine : Binding (Result x a) -> Result x (Binding a)
combine { name, body } =
    Result.map
        (\body_ ->
            { name = name
            , body = body_
            }
        )
        body
