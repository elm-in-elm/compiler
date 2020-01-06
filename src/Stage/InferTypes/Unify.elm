module Stage.InferTypes.Unify exposing
    ( unify
    , unifyAllEquations
    )

import Dict
import Elm.Compiler.Error exposing (TypeError(..))
import Elm.Data.Type as Type exposing (Type(..))
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


unify : Type -> Type -> SubstitutionMap -> Result ( TypeError, SubstitutionMap ) SubstitutionMap
unify t1 t2 substitutionMap =
    if t1 == t2 then
        Ok substitutionMap

    else
        case ( t1, t2 ) of
            {- Do not use joker pattern "_" at the end of this case and
                pattern match all the types couples with

                 ( T a b, T c d) ->
                     ...

                 ( T _ _, _) ->
                     Err ( TypeMismatch t1 t2, substitutionMap )

               It is definitively verbose but prevents the developer to skip this important
               function: the compiler will throw a "MISSING PATTERNS" error in this case.
            -}
            ( Var id, _ ) ->
                unifyVariable id t2 substitutionMap

            ( _, Var id ) ->
                unifyVariable id t1 substitutionMap

            ( Int, Int ) ->
                Ok substitutionMap

            ( Int, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( Float, Float ) ->
                Ok substitutionMap

            ( Float, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( String, String ) ->
                Ok substitutionMap

            ( String, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( Char, Char ) ->
                Ok substitutionMap

            ( Char, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( Bool, Bool ) ->
                Ok substitutionMap

            ( Bool, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( Unit, Unit ) ->
                Ok substitutionMap

            ( Unit, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( Function a, Function b ) ->
                unify a.to b.to substitutionMap
                    |> Result.andThen (unify a.from b.from)

            ( Function _, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( List list1, List list2 ) ->
                unify list1 list2 substitutionMap

            ( List _, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( Tuple t1e1 t1e2, Tuple t2e1 t2e2 ) ->
                substitutionMap
                    |> unify t1e1 t2e1
                    |> Result.andThen (unify t1e2 t2e2)

            ( Tuple _ _, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( Tuple3 t1e1 t1e2 t1e3, Tuple3 t2e1 t2e2 t2e3 ) ->
                substitutionMap
                    |> unify t1e1 t2e1
                    |> Result.andThen (unify t1e2 t2e2)
                    |> Result.andThen (unify t1e3 t2e3)

            ( Tuple3 _ _ _, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

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

            ( Record _, _ ) ->
                Err ( TypeMismatch t1 t2, substitutionMap )

            ( UserDefinedType typeName1 typeParameters1, UserDefinedType typeName2 typeParameters2 ) ->
                if typeName1 /= typeName2 || List.length typeParameters1 /= List.length typeParameters2 then
                    Err ( TypeMismatch t1 t2, substitutionMap )

                else
                    List.map2 Tuple.pair typeParameters1 typeParameters2
                        |> List.foldl
                            (\( type1, type2 ) resultSubstitutionMap ->
                                resultSubstitutionMap
                                    |> Result.andThen (unify type1 type2)
                            )
                            (Ok substitutionMap)

            ( UserDefinedType _ _, _ ) ->
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
                    Function { from, to } ->
                        occurs id to substitutionMap
                            || occurs id from substitutionMap

                    -- TODO potentially dangerous wildcard?
                    _ ->
                        False
