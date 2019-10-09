module OurExtras.Dict exposing (combine)

import Dict exposing (Dict)


{-| Similar to Result.Extra.combine which works for Lists.
-}
combine : Dict comparable (Result err v) -> Result err (Dict comparable v)
combine dictOfResults =
    dictOfResults
        |> Dict.foldr
            {- Is this readable enough? We're creating a new dict with the same
               key but the contents of the Results, not the Results themselves.
            -}
            (Result.map2 << Dict.insert)
            (Ok Dict.empty)
