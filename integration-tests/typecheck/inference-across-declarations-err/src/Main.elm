module Main exposing (main)


incr : Int -> Int
incr =
    \x -> x + 1


a : Int
a =
    incr True


main : Int
main =
    a
