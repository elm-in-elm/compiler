module Main exposing (main)

{-| Doc comment
-}

-- Hello
{- and multi
   line
   comment
-}


{-| Another doc comment
With
{- nested comment -}
heyoo
-}
type alias Record =
    { a : Int, b : Int }


main =
    -123 + (Record 1 2).a
