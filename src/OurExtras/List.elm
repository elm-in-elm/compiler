module OurExtras.List exposing
    ( fastConcat
    , fastConcatMap
    )

{-| Our own List.Extra module.
-}


{-| A faster List.concat alternative.
TODO could be removed if/after <https://github.com/elm/core/pull/1027> is merged
and we bump to elm/core with that "fix".
-}
fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


{-| List.concatMap alternative using our `fastConcat`.
-}
fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap f =
    List.foldr (f >> (++)) []
