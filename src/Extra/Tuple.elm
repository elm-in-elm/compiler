module Extra.Tuple exposing (third)


third : ( a, b, c ) -> c
third ( _, _, c ) =
    c
