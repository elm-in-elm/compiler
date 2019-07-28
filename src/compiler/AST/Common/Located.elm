module AST.Common.Located exposing
    ( Located
    , Region
    , dummyRegion
    , getRegion
    , located
    , map
    , merge
    , parsed
    , replaceWith
    , unwrap
    )


type Located expr
    = Located Region expr


type alias Region =
    { start : Position
    , end : Position
    }


type alias Position =
    { row : Int
    , col : Int
    }


{-| arguments order to facilitate parsing, see Parser.located
-}
parsed : ( Int, Int ) -> a -> ( Int, Int ) -> Located a
parsed ( startRow, startCol ) value ( endRow, endCol ) =
    Located
        { start = Position startRow startCol
        , end = Position endRow endCol
        }
        value


located : Region -> expr -> Located expr
located =
    Located


unwrap : Located a -> a
unwrap (Located _ expr) =
    expr


map : (a -> b) -> Located a -> Located b
map fn (Located region expr) =
    Located region (fn expr)


replaceWith : b -> Located a -> Located b
replaceWith expr (Located region _) =
    Located region expr


merge : (Located a -> Located a -> b) -> Located a -> Located a -> Located b
merge fn l1 l2 =
    Located
        (mergeRegions (getRegion l1) (getRegion l2))
        (fn l1 l2)


getRegion : Located a -> Region
getRegion (Located region _) =
    region


mergeRegions : Region -> Region -> Region
mergeRegions r1 r2 =
    { start = r1.start
    , end = r2.end
    }


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
