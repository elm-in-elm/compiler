module Shrink.Extra exposing (listWithoutEmptying, map2, mergeMany, singleton)

import Shrink exposing (Shrinker)


{-| Combines two lazy lists using a combining function.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    lazyMap2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c

-}
map2 f la lb =
    Shrink.map f la
        |> Shrink.andMap lb


{-| Produces a single element lazy list.

---

We cannot write a type annotation here.
The `LazyList a` type used by shrinkers is not exposed outside `elm-explorations/test`.

    singleton : a -> LazyList a

-}
singleton x =
    x |> const x


const : a -> Shrinker a
const shrunk _ =
    True
        |> Shrink.bool
        |> Shrink.map (always shrunk)


mergeMany : List (Shrinker a) -> Shrinker a
mergeMany shrinkers =
    shrinkers
        |> List.foldl Shrink.merge Shrink.noShrink


{-| Given a shrinker of elements, produces a shrinker of lists.

This is different from `Shrink.list` in that it keeps non-empty lists non-empty.

So if you try shriking `[]` you will get nothing.
And if you try shrinking `[1]` you might get `[0]`, but you will not get `[]`.

-}
listWithoutEmptying : Shrinker a -> Shrinker (List a)
listWithoutEmptying shrinkElement list =
    case list of
        first :: rest ->
            map2 (::)
                (shrinkElement first)
                (Shrink.list shrinkElement rest)

        [] ->
            Shrink.noShrink list
