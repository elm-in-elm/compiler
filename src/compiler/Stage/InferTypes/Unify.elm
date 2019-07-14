module Stage.InferTypes.Unify exposing (unifyAllEquations)

import AST.Common.Type as Type exposing (Type)
import Error exposing (TypeError(..))
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Stage.InferTypes.TypeEquation as TypeEquation exposing (TypeEquation)


{-| TODO document
-}
unifyAllEquations : List TypeEquation -> Result TypeError SubstitutionMap
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


unify : Type -> Type -> SubstitutionMap -> Result TypeError SubstitutionMap
unify t1 t2 substitutionMap =
    if t1 == t2 then
        Ok substitutionMap

    else
        case ( t1, t2 ) of
            ( Type.Var id, _ ) ->
                unifyVariable id t2 substitutionMap

            ( _, Type.Var id ) ->
                unifyVariable id t1 substitutionMap

            ( Type.Function arg1 result1, Type.Function arg2 result2 ) ->
                unify result1 result2 substitutionMap
                    |> Result.andThen (unify arg1 arg2)

            ( Type.List list1, Type.List list2 ) ->
                unify list1 list2 substitutionMap

            _ ->
                Err (TypeMismatch t1 t2)


unifyVariable : Int -> Type -> SubstitutionMap -> Result TypeError SubstitutionMap
unifyVariable id type_ substitutionMap =
    case SubstitutionMap.get id substitutionMap of
        Just typeForId ->
            unify typeForId type_ substitutionMap

        Nothing ->
            case
                Type.getVarId type_
                    |> Maybe.andThen (\id2 -> SubstitutionMap.get id2 substitutionMap)
                    |> Maybe.map (\typeForId2 -> unify (Type.Var id) typeForId2 substitutionMap)
            of
                Just newSubstitutionMap ->
                    newSubstitutionMap

                Nothing ->
                    if occurs id type_ substitutionMap then
                        Err (OccursCheckFailed id type_)

                    else
                        Ok (SubstitutionMap.insert id type_ substitutionMap)


occurs : Int -> Type -> SubstitutionMap -> Bool
occurs id type_ substitutionMap =
    if type_ == Type.Var id then
        True

    else
        case
            Type.getVarId type_
                |> Maybe.andThen (\id2 -> SubstitutionMap.get id2 substitutionMap)
                |> Maybe.map (\typeForId2 -> occurs id typeForId2 substitutionMap)
        of
            Just doesOccur ->
                doesOccur

            Nothing ->
                case type_ of
                    Type.Function arg result ->
                        occurs id result substitutionMap
                            || occurs id arg substitutionMap

                    -- TODO potentially dangerous wildcard?
                    _ ->
                        False
