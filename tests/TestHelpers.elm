module TestHelpers exposing
    ( located
    , typed
    , typedBool
    , typedInt
    , typedString
    )

import AST.Common.Literal as Literal
import AST.Common.Located as Located exposing (Located)
import AST.Common.Type exposing (Type(..))
import AST.Typed as Typed exposing (Expr_(..))


{-| For when the location and type doesn't matter
-}
typed : Typed.Expr_ -> Typed.LocatedExpr
typed expr_ =
    Located.located
        dummyRegion
        ( expr_, Int )


{-| For when the location doesn't matter
-}
located : expr -> Located expr
located expr =
    Located.located
        dummyRegion
        expr


typedInt : Int -> Typed.LocatedExpr
typedInt int =
    located
        ( Literal (Literal.Int int)
        , Int
        )


typedBool : Bool -> Typed.LocatedExpr
typedBool bool =
    located
        ( Literal (Literal.Bool bool)
        , Bool
        )


typedString : String -> Typed.LocatedExpr
typedString str =
    located
        ( Literal (Literal.String str)
        , String
        )


dummyRegion : Located.Region
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
