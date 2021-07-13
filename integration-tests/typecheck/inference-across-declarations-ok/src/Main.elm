module Main exposing (..)


someFunction : Int -> a -> Int
someFunction =
    \a b -> 3 + a


main : a -> Int
main =
    someFunction 5
