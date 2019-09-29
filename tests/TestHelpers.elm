module TestHelpers exposing
    ( dumpType
    , located
    , typed
    , typedBool
    , typedInt
    , typedIntList
    , typedString
    )

import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Type as Type exposing (Type)
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
        ( expr_, Type.Int )


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
        ( Int int
        , Type.Int
        )


typedBool : Bool -> Typed.LocatedExpr
typedBool bool =
    located
        ( Bool bool
        , Type.Bool
        )


typedString : String -> Typed.LocatedExpr
typedString str =
    located
        ( String str
        , Type.String
        )


typedIntList : List Int -> Typed.LocatedExpr
typedIntList list =
    located
        ( List (List.map typedInt list)
        , Type.List Type.Int
        )
