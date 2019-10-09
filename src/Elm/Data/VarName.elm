module Elm.Data.VarName exposing (VarName)

{-| Variable name, eg. the `x` in

    foo =
        x + 1

@docs VarName

-}


{-| Just a `String` alias, instead of a `type` wrapper. We generally use records
with explanatory field names where two Strings would be next to each other,
to protect against swapping them accidentally.
-}
type alias VarName =
    String
