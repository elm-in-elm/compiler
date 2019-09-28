module TestHelpers exposing
    ( dumpType
    , located
    , typed
    , typedBool
    , typedInt
    , typedIntList
    , typedString
    )

import Elm.AST.Common.Literal as Literal
import Elm.AST.Common.Located as Located exposing (Located)
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Type as Type exposing (Type(..))
import Elm.Data.Type.ToString as TypeToString


dumpType : Type -> String
dumpType type_ =
    type_
        |> TypeToString.toString TypeToString.emptyState
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
        ( Typed.List (List.map typedInt list)
        , Type.List Type.Int
        )
