module Elm.Data.VarName exposing (VarName)

{-| Variable name or a type name, eg. the 'A' and the `x` in

    foo : A
    foo =
        bar x

@docs VarName

-}


{-| Just a `String` alias, instead of a `type` wrapper. We generally use records
with explanatory field names where two Strings would be next to each other,
to protect against swapping them accidentally.
-}
type alias VarName =
    String
