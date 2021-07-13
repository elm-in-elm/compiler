module Main exposing (..)


foo : Int -> a
foo =
    \x -> bar x


bar : Int -> a
bar =
    \x -> foo x


main =
    foo 5
