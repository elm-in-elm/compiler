module Main exposing (main)


type alias Record =
    { a : Int, b : Int }


main =
    1 + (Record 1 2).a
