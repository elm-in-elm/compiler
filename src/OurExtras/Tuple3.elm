module OurExtras.Tuple3 exposing (first, mapFirst, mapSecond, mapThird, second, third)


first : ( a, b, c ) -> a
first ( a, _, _ ) =
    a


second : ( a, b, c ) -> b
second ( _, b, _ ) =
    b


third : ( a, b, c ) -> c
third ( _, _, c ) =
    c


mapFirst : (a1 -> a2) -> ( a1, b, c ) -> ( a2, b, c )
mapFirst fn ( a, b, c ) =
    ( fn a, b, c )


mapSecond : (b1 -> b2) -> ( a, b1, c ) -> ( a, b2, c )
mapSecond fn ( a, b, c ) =
    ( a, fn b, c )


mapThird : (c1 -> c2) -> ( a, b, c1 ) -> ( a, b, c2 )
mapThird fn ( a, b, c ) =
    ( a, b, fn c )
