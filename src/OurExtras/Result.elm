module OurExtras.Result exposing (combineNonEmpty)

import List.NonEmpty exposing (NonEmpty)
import Result.Extra as Result


combineNonEmpty : NonEmpty (Result err a) -> Result err (NonEmpty a)
combineNonEmpty ( x, xs ) =
    Result.map2 Tuple.pair
        x
        (Result.combine xs)
