module AST.Common exposing (Literal(..))

{- Get rid of this (or at least rename well!) the second literals change
   between the stages!
-}


type Literal
    = Int Int
    | Char Char
    | String String
