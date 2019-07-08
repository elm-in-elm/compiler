module AST.Common.Literal exposing (Literal(..))


type Literal
    = Int Int
    | Float Float
    | Char Char
    | String String
    | Bool Bool
