module Main exposing (main)


main =
    call 1


call =
    \x -> x + 2


ifTest =
    if 1 + 2 then
        call 3

    else
        4 + call 4
