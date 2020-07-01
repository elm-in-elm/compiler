module TestHelpers exposing
    ( dumpConcreteType
    , dumpType
    , dumpTypeOrId
    , located
    , typed
    , typedBool
    , typedInt
    , typedIntList
    , typedString
    )

import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Located as Located exposing (Located)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type exposing (Type, TypeOrId(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.Type.ToString as TypeToString


dumpTypeOrId : TypeOrId Qualified -> String
dumpTypeOrId typeOrId =
    typeOrId
        |> TypeToString.toString (TypeToString.fromTypeOrId typeOrId)
        |> Tuple.first


dumpType : Type Qualified -> String
dumpType type_ =
    type_
        |> TypeToString.toStringType (TypeToString.fromType type_)
        |> Tuple.first


dumpConcreteType : ConcreteType Qualified -> String
dumpConcreteType concreteType =
    concreteType
        |> ConcreteType.toType
        |> dumpType


{-| For when the location and type doesn't matter
-}
typed : Typed.Expr_ -> Typed.LocatedExpr
typed expr_ =
    Located.located
        Located.dummyRegion
        ( expr_, Type Type.Int )


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
        , Type Type.Int
        )


typedBool : Bool -> Typed.LocatedExpr
typedBool bool =
    located
        ( Bool bool
        , Type Type.Bool
        )


typedString : String -> Typed.LocatedExpr
typedString str =
    located
        ( String str
        , Type Type.String
        )


typedIntList : List Int -> Typed.LocatedExpr
typedIntList list =
    located
        ( List (List.map typedInt list)
        , Type <| Type.List (Type Type.Int)
        )
