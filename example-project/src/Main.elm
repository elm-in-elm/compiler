module Main exposing (main)

import AndTheLastOne exposing (b)
import Nested.Test as NT
import Other
import YetAnother exposing (..)


main =
    x + 42 + Other.y + NT.z + a + b + lambda + anotherLambda


lambda =
    \abc -> abc + 2


anotherLambda =
    \abc def -> abc + def


x =
    1
