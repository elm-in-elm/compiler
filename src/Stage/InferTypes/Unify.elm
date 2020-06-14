module Stage.InferTypes.Unify exposing
    ( unify
    , unifyAllEquations
    )

import Dict
import Elm.Compiler.Error exposing (TypeError(..))
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type as Type exposing (Type(..), TypeOrId(..))
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Stage.InferTypes.TypeEquation as TypeEquation exposing (TypeEquation)


{-| TODO document
-}
unifyAllEquations : List TypeEquation -> SubstitutionMap -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unifyAllEquations equations substitutionMap =
    List.foldl
        (\equation substitutionMap_ ->
            let
                ( t1, t2 ) =
                    TypeEquation.unwrap equation
            in
            Result.andThen (unify t1 t2) substitutionMap_
        )
        (Ok substitutionMap)
        equations


unify : TypeOrId Qualified -> TypeOrId Qualified -> SubstitutionMap -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unify t1 t2 substitutionMap =
    if t1 == t2 then
        Ok substitutionMap

    else
        case ( t1, t2 ) of
            ( Id id, _ ) ->
                unifyVariable id t2 substitutionMap

            ( _, Id id ) ->
                unifyVariable id t1 substitutionMap

            ( Type t1_, Type t2_ ) ->
                unifyTypes t1_ t2_ substitutionMap


unifyTypes : Type Qualified -> Type Qualified -> SubstitutionMap -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unifyTypes t1 t2 substitutionMap =
    let
        err =
            Err ( TypeMismatch (Type t1) (Type t2), substitutionMap )
    in
    case ( t1, t2 ) of
        ( Var name1, Var name2 ) ->
            if name1 == name2 then
                Ok substitutionMap

            else
                -- TODO is this correct?
                err

        ( Var _, _ ) ->
            err

        ( Int, Int ) ->
            Ok substitutionMap

        ( Int, _ ) ->
            err

        ( Float, Float ) ->
            Ok substitutionMap

        ( Float, _ ) ->
            err

        ( String, String ) ->
            Ok substitutionMap

        ( String, _ ) ->
            err

        ( Char, Char ) ->
            Ok substitutionMap

        ( Char, _ ) ->
            err

        ( Bool, Bool ) ->
            Ok substitutionMap

        ( Bool, _ ) ->
            err

        ( Unit, Unit ) ->
            Ok substitutionMap

        ( Unit, _ ) ->
            err

        ( Function a, Function b ) ->
            Ok substitutionMap
                |> Result.andThen (unify a.to b.to)
                |> Result.andThen (unify a.from b.from)

        ( Function _, _ ) ->
            err

        ( List list1, List list2 ) ->
            unify list1 list2 substitutionMap

        ( List _, _ ) ->
            err

        ( Tuple t1e1 t1e2, Tuple t2e1 t2e2 ) ->
            Ok substitutionMap
                |> Result.andThen (unify t1e1 t2e1)
                |> Result.andThen (unify t1e2 t2e2)

        ( Tuple _ _, _ ) ->
            err

        ( Tuple3 t1e1 t1e2 t1e3, Tuple3 t2e1 t2e2 t2e3 ) ->
            Ok substitutionMap
                |> Result.andThen (unify t1e1 t2e1)
                |> Result.andThen (unify t1e2 t2e2)
                |> Result.andThen (unify t1e3 t2e3)

        ( Tuple3 _ _ _, _ ) ->
            err

        ( Record bindings1, Record bindings2 ) ->
            if Dict.keys bindings1 /= Dict.keys bindings2 then
                Err ( TypeMismatch (Type t1) (Type t2), substitutionMap )

            else
                List.map2 Tuple.pair (Dict.values bindings1) (Dict.values bindings2)
                    |> List.foldl
                        (\( type1, type2 ) resultSubstitutionMap ->
                            resultSubstitutionMap
                                |> Result.andThen (unify type1 type2)
                        )
                        (Ok substitutionMap)

        ( Record _, _ ) ->
            err

        ( UserDefinedType ut1, UserDefinedType ut2 ) ->
            if ut1.name /= ut2.name || List.length ut1.args /= List.length ut2.args then
                Err ( TypeMismatch (Type t1) (Type t2), substitutionMap )

            else
                List.map2 Tuple.pair ut1.args ut2.args
                    |> List.foldl
                        (\( type1, type2 ) resultSubstitutionMap ->
                            resultSubstitutionMap
                                |> Result.andThen (unify type1 type2)
                        )
                        (Ok substitutionMap)

        ( UserDefinedType _, _ ) ->
            err


unifyVariable : Int -> TypeOrId Qualified -> SubstitutionMap -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unifyVariable id otherTypeOrId substitutionMap =
    case SubstitutionMap.get id substitutionMap of
        Just typeOrId ->
            unify typeOrId otherTypeOrId substitutionMap

        Nothing ->
            case
                Type.getId otherTypeOrId
                    |> Maybe.andThen (\id2 -> SubstitutionMap.get id2 substitutionMap)
                    |> Maybe.map (\typeOrId2 -> unifyVariable id typeOrId2 substitutionMap)
            of
                Just result ->
                    result

                Nothing ->
                    if occurs id otherTypeOrId substitutionMap then
                        Err ( OccursCheckFailed id otherTypeOrId, substitutionMap )

                    else
                        Ok (SubstitutionMap.insert id otherTypeOrId substitutionMap)


occurs : Int -> TypeOrId Qualified -> SubstitutionMap -> Bool
occurs id typeOrId substitutionMap =
    let
        f : TypeOrId Qualified -> Bool
        f typeOrId_ =
            occurs id typeOrId_ substitutionMap
    in
    case typeOrId of
        Id id_ ->
            -- TODO ??? should this be False instead?
            id == id_

        Type type_ ->
            case type_ of
                Var _ ->
                    -- TODO ??? what if var "a" got mapped to id 0? we should maybe check some mapping String->Int?
                    False

                Function { from, to } ->
                    f from || f to

                Int ->
                    False

                Float ->
                    False

                Char ->
                    False

                String ->
                    False

                Bool ->
                    False

                List listType ->
                    f listType

                Unit ->
                    False

                Tuple t1 t2 ->
                    f t1 || f t2

                Tuple3 t1 t2 t3 ->
                    f t1 || f t2 || f t3

                Record bindings ->
                    bindings
                        |> Dict.values
                        |> List.any f

                UserDefinedType { args } ->
                    List.any f args
