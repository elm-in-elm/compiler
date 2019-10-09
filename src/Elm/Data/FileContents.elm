module Elm.Data.FileContents exposing (FileContents)

{-| File contents, most often source code.

@docs FileContents

-}


{-| Just a `String` alias, instead of a `type` wrapper. We generally use records
with explanatory field names where two Strings would be next to each other,
to protect against swapping them accidentally.
-}
type alias FileContents =
    String
