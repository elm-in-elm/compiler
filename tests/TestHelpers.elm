module TestHelpers exposing
    ( dumpType
    , located
    , typed
    , typedBool
    , typedInt
    , typedIntList
    , typedString
    )

import AST.Common.Literal as Literal
import AST.Common.Located as Located exposing (Located)
import AST.Common.Type as Type exposing (Type(..))
import AST.Typed as Typed exposing (Expr_(..))


dumpType : Type -> String
dumpType type_ =
    type_
        |> Type.toString Type.emptyState
        |> Tuple.first


{-| For when the location and type doesn't matter
-}
typed : Typed.Expr_ -> Typed.LocatedExpr
typed expr_ =
    Located.located
        Located.dummyRegion
        ( expr_, Int )


{-| For when the location doesn't matter
-}
located : expr -> Located expr
located expr =
    Located.located
        Located.dummyRegion
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


typedIntList : List Int -> Typed.LocatedExpr
typedIntList list =
    located
        ( Typed.List
            ([ 1, 2, 3 ]
                |> List.map typedInt
            )
        , Type.List Type.Int
        )
