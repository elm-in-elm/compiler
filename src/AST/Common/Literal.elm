module AST.Common.Literal exposing (Literal(..))


type Literal
    = Int Int
    | Char Char
    | String String
    | Bool Bool
