module LocatedTest exposing (mergeRegions)

import Elm.Data.Located as Located exposing (Position, Region)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2)


mergeRegions : Test
mergeRegions =
    describe "Elm.Data.Located.mergeRegions"
        [ fuzz2 region region "Always bigger or equal than input regions" <|
            \r1 r2 ->
                Located.mergeRegions r1 r2
                    |> Expect.all
                        [ \{ start } -> Located.comparePosition start r1.start |> Expect.notEqual GT
                        , \{ start } -> Located.comparePosition start r2.start |> Expect.notEqual GT
                        , \{ end } -> Located.comparePosition end r1.end |> Expect.notEqual LT
                        , \{ end } -> Located.comparePosition end r2.end |> Expect.notEqual LT
                        ]
        , fuzz2 region region "Order doesn't matter" <|
            \r1 r2 ->
                Located.mergeRegions r1 r2
                    |> Expect.equal (Located.mergeRegions r2 r1)
        ]


region : Fuzzer Region
region =
    Fuzz.map2 Region
        position
        position


position : Fuzzer Position
position =
    Fuzz.map2 Position
        Fuzz.int
        Fuzz.int
