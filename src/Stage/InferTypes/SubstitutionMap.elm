module Stage.InferTypes.SubstitutionMap exposing
    ( SubstitutionMap
    , empty
    , get
    , insert
    , isEmpty
    )

import AST.Common.Type exposing (Type)
import Dict exposing (Dict)


{-| A thin opaque wrapper around a dict from type variable IDs to inferred types.
-}
type SubstitutionMap
    = SubstitutionMap (Dict Int Type)


empty : SubstitutionMap
empty =
    SubstitutionMap Dict.empty


insert : Int -> Type -> SubstitutionMap -> SubstitutionMap
insert id type_ (SubstitutionMap substitutionMap) =
    SubstitutionMap (Dict.insert id type_ substitutionMap)


get : Int -> SubstitutionMap -> Maybe Type
get id (SubstitutionMap substitutionMap) =
    Dict.get id substitutionMap


isEmpty : SubstitutionMap -> Bool
isEmpty (SubstitutionMap substitutionMap) =
    Dict.isEmpty substitutionMap
