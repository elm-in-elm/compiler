module Main exposing (main)


f =
    \x ->
        if x then
            18

        else
            2


main =
    f True + f False
