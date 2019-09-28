module Elm.Data.Binding exposing
    ( Binding
    , combine
    , map
    )


type alias Binding expr =
    { name : String
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
