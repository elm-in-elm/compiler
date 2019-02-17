module Main exposing (x)

import AndTheLastOne exposing (b)
import Nested.Test as NT
import Other
import YetAnother exposing (..)


main =
    x + 42 + Other.y + NT.z + a + b


x =
    1
