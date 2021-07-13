module Stage.InferTypes.SubstitutionMap exposing
    ( SubstitutionMap
    , empty
    , get
    , insert
    , isEmpty
    )

import Dict exposing (Dict)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type exposing (Id, TypeOrId(..))


{-| A thin opaque wrapper around a dict from type variable IDs to inferred types.
Note ID can point to another ID (eg. dict entry `(1,Id 2)`) so you might need to
walk this dict multiple times.
-}
type SubstitutionMap
    = SubstitutionMap (Dict Id (TypeOrId Qualified))


empty : SubstitutionMap
empty =
    SubstitutionMap Dict.empty


{-| Automatically walks the TypeOrId end of the mapping if it's an ID.

    insert 1 (Id 2) (SubstitutionMap [(2,Type Unit)])
    --> SubstitutionMap [(1,Type Unit),(2,Type Unit)]

-}
insert : Id -> TypeOrId Qualified -> SubstitutionMap -> SubstitutionMap
insert id typeOrId ((SubstitutionMap substitutionMap) as map) =
    case typeOrId of
        Id id_ ->
            case get id_ map of
                Nothing ->
                    SubstitutionMap (Dict.insert id typeOrId substitutionMap)

                Just another ->
                    insert id another map

        Type _ ->
            SubstitutionMap (Dict.insert id typeOrId substitutionMap)


get : Id -> SubstitutionMap -> Maybe (TypeOrId Qualified)
get id (SubstitutionMap substitutionMap) =
    Dict.get id substitutionMap


isEmpty : SubstitutionMap -> Bool
isEmpty (SubstitutionMap substitutionMap) =
    Dict.isEmpty substitutionMap
