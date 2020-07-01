module OurExtras.List.NonEmpty exposing
    ( combine
    , fastConcat
    , fastConcatMap
    )

import List.NonEmpty exposing (NonEmpty)
import OurExtras.List as List
import Result.Extra


combine : NonEmpty (Result a b) -> Result a (NonEmpty b)
combine ( x, xs ) =
    Result.map2 List.NonEmpty.fromCons x (Result.Extra.combine xs)


{-| Concatenate a bunch of lists into a single list.
concat ((1, [2, 3]), [(4, [5, 6]), (7, [8]), (9, []), (10, [11])])
--> (1,[2,3,4,5,6,7,8,9,10,11])
-}
fastConcat : NonEmpty (NonEmpty a) -> NonEmpty a
fastConcat ( h, t ) =
    let
        hx =
            List.NonEmpty.head h

        tx =
            List.NonEmpty.tail h ++ List.fastConcat (List.map List.NonEmpty.toList t)
    in
    ( hx, tx )


{-| Map a given function onto a list and flatten the resulting lists.
concatMap singleton ( 1, [ 2 ] )
--> ( 1, [ 2 ] )
concatMap (\\x -> ( x + 1, [ x + 1 ] )) ( 1, [ 2 ] )
--> ( 2, [ 2, 3, 3 ] )
-}
fastConcatMap : (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
fastConcatMap f =
    fastConcat << List.NonEmpty.map f
