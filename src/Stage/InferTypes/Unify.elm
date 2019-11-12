module Stage.InferTypes.Unify exposing (unifyAllEquations)

import Dict
import Elm.Compiler.Error exposing (TypeError(..))
import Elm.Data.Type as Type exposing (Type(..))
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Stage.InferTypes.TypeEquation as TypeEquation exposing (TypeEquation)


{-| TODO document
-}
unifyAllEquations : List TypeEquation -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unifyAllEquations equations =
    List.foldl
        (\equation substitutionMap ->
            let
                ( t1, t2 ) =
                    TypeEquation.unwrap equation
            in
            Result.andThen (unify t1 t2) substitutionMap
        )
        (Ok SubstitutionMap.empty)
        equations


unify : Type -> Type -> SubstitutionMap -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unify t1 t2 substitutionMap =
    if t1 == t2 then
        Ok substitutionMap

    else
        case ( t1, t2 ) of
            ( Var id, _ ) ->
                unifyVariable id t2 substitutionMap

            ( _, Var id ) ->
                unifyVariable id t1 substitutionMap

            ( Function arg1 result1, Function arg2 result2 ) ->
                unify result1 result2 substitutionMap
                    |> Result.andThen (unify arg1 arg2)

            ( List list1, List list2 ) ->
                unify list1 list2 substitutionMap

            ( Tuple t1e1 t1e2, Tuple t2e1 t2e2 ) ->
                substitutionMap
                    |> unify t1e1 t2e1
                    |> Result.andThen (unify t1e2 t2e2)

            ( Tuple3 t1e1 t1e2 t1e3, Tuple3 t2e1 t2e2 t2e3 ) ->
                substitutionMap
                    |> unify t1e1 t2e1
                    |> Result.andThen (unify t1e2 t2e2)
                    |> Result.andThen (unify t1e3 t2e3)

            ( Record bindings1, Record bindings2 ) ->
                if Dict.keys bindings1 /= Dict.keys bindings2 then
                    Err ( TypeMismatch t1 t2, substitutionMap )

                else
                    List.map2 Tuple.pair (Dict.values bindings1) (Dict.values bindings2)
                        |> List.foldl
                            (\( type1, type2 ) resultSubstitutionMap ->
                                resultSubstitutionMap
                                    |> Result.andThen (unify type1 type2)
                            )
                            (Ok substitutionMap)

            _ ->
                Err ( TypeMismatch t1 t2, substitutionMap )


unifyVariable : Int -> Type -> SubstitutionMap -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unifyVariable id type_ substitutionMap =
    case SubstitutionMap.get id substitutionMap of
        Just typeForId ->
            unify typeForId type_ substitutionMap

        Nothing ->
            case
                Type.varId type_
                    |> Maybe.andThen (\id2 -> SubstitutionMap.get id2 substitutionMap)
                    |> Maybe.map (\typeForId2 -> unify (Var id) typeForId2 substitutionMap)
            of
                Just newSubstitutionMap ->
                    newSubstitutionMap

                Nothing ->
                    if occurs id type_ substitutionMap then
                        Err ( OccursCheckFailed id type_, substitutionMap )

                    else
                        Ok (SubstitutionMap.insert id type_ substitutionMap)


occurs : Int -> Type -> SubstitutionMap -> Bool
occurs id type_ substitutionMap =
    if type_ == Var id then
        True

    else
        case
            Type.varId type_
                |> Maybe.andThen (\id2 -> SubstitutionMap.get id2 substitutionMap)
                |> Maybe.map (\typeForId2 -> occurs id typeForId2 substitutionMap)
        of
            Just doesOccur ->
                doesOccur

            Nothing ->
                case type_ of
                    Function arg result ->
                        occurs id result substitutionMap
                            || occurs id arg substitutionMap

                    -- TODO potentially dangerous wildcard?
                    _ ->
                        False
