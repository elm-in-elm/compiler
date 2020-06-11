module Elm.Data.Located exposing
    ( Located, Region, Position
    , located, unwrap, getRegion, map, merge, replaceWith
    , dummyRegion, mergeRegions, regionToComparable
    , positionToComparable, comparePosition
    )

{-| Wrapper for location metadata. The location is essentially a pair of
`(row,col)` coordinates for some span of source code.

Useful for error messages, but hopefully for stuff like source maps etc. too.


# Types

@docs Located, Region, Position


# Located

@docs located, unwrap, getRegion, map, merge, replaceWith


# Region

@docs dummyRegion, mergeRegions, regionToComparable


# Position

@docs positionToComparable, comparePosition

-}


{-| Holds location metadata and some value.
-}
type Located expr
    = Located Region expr


{-| -}
type alias Region =
    { start : Position
    , end : Position
    }


{-| -}
type alias Position =
    { row : Int
    , col : Int
    }


{-| A constructor for the Located type.
-}
located : Region -> expr -> Located expr
located =
    Located


{-| Return the value inside the wrapper.
-}
unwrap : Located a -> a
unwrap (Located _ expr) =
    expr


{-| Return the location info inside the wrapper.
-}
getRegion : Located a -> Region
getRegion (Located region _) =
    region


{-| Apply a function to the value inside the wrapper.
-}
map : (a -> b) -> Located a -> Located b
map fn (Located region expr) =
    Located region (fn expr)


{-| Replace the value inside the wrapper with another.
-}
replaceWith : b -> Located a -> Located b
replaceWith expr (Located region _) =
    Located region expr


{-| Merge the regions of the two wrappers.
-}
merge : (Located a -> Located b -> c) -> Located a -> Located b -> Located c
merge fn l1 l2 =
    Located
        (mergeRegions (getRegion l1) (getRegion l2))
        (fn l1 l2)


{-| Merge the regions: the resulting region is always bigger or equal than the
input regions.

    mergeRegions <1:1 - 4:4> <2:2 - 3:3>
        --> <1:1 - 4:4>

The order doesn't matter:

    mergeRegions <2:2 - 3:3> <1:1 - 4:4>
        --> <1:1 - 4:4>

One doesn't have to be a subset of the other:

    mergeRegions <1:1 - 3:3> <2:2 - 4:4>
        --> <1:1 - 4:4>

There can be gaps in between

    mergeRegions <1:1 - 2:2> <4:4 - 5:5>
        --> <1:1 - 5:5>

-}
mergeRegions : Region -> Region -> Region
mergeRegions r1 r2 =
    { start = minPosition r1.start r2.start
    , end = maxPosition r1.end r2.end
    }


minPosition : Position -> Position -> Position
minPosition p1 p2 =
    case compare ( p1.row, p1.col ) ( p2.row, p2.col ) of
        LT ->
            p1

        EQ ->
            p1

        GT ->
            p2


maxPosition : Position -> Position -> Position
maxPosition p1 p2 =
    case compare ( p1.row, p1.col ) ( p2.row, p2.col ) of
        LT ->
            p2

        EQ ->
            p1

        GT ->
            p1


{-| Empty, meaningless region. Used in tests or where you don't need the location
info but need to appease the typesystem.
-}
dummyRegion : Region
dummyRegion =
    { start =
        { row = 0
        , col = 0
        }
    , end =
        { row = 0
        , col = 0
        }
    }


{-| Transforms the record into something `comparable`.
-}
regionToComparable : Region -> ( ( Int, Int ), ( Int, Int ) )
regionToComparable { start, end } =
    ( positionToComparable start
    , positionToComparable end
    )


{-| Transforms the record into something `comparable`.
-}
positionToComparable : Position -> ( Int, Int )
positionToComparable { row, col } =
    ( row, col )


{-| Compare using [`positionToComparable`](#positionToComparable)
-}
comparePosition : Position -> Position -> Order
comparePosition p1 p2 =
    compare
        (positionToComparable p1)
        (positionToComparable p2)
