module Stage.InferTypes.IdSource exposing
    ( IdGenerator
    , fresh
    , freshWith1AndMultiple
    , freshWith1AndVar
    , freshWith2
    , freshWith3
    , generate
    , rememberVar
    )

{-| A boilerplate-reducing abstraction for the `assignIds` stage of InferTypes stage.
-}

import AST.Common.Type as Type exposing (Type)
import Common
import Common.Types exposing (VarName)
import Dict.Any exposing (AnyDict)
import Error exposing (TypeError(..))


{-| TODO better names? Constant instead of Fresh?

TODO any idea about how to make this more generic? (something like andMap?)

-}
type IdGenerator expr
    = Fresh expr
    | FreshWith1AndMultiple (List ( expr, Id ) -> ( expr, Id ) -> expr) (List (IdGenerator expr)) (IdGenerator expr)
    | FreshWith1AndVar (( expr, Id ) -> Int -> expr) (IdGenerator expr) VarName
    | FreshWith2 (( expr, Id ) -> ( expr, Id ) -> expr) (IdGenerator expr) (IdGenerator expr)
    | FreshWith3 (( expr, Id ) -> ( expr, Id ) -> ( expr, Id ) -> expr) (IdGenerator expr) (IdGenerator expr) (IdGenerator expr)
    | RememberVar VarName (IdGenerator expr)


type alias VarIds =
    AnyDict String VarName Int


type alias Output a =
    Result TypeError (OutputData a)


type alias OutputData a =
    { expr : a
    , exprRawId : Int
    , unusedId : Int
    , varIds : VarIds
    }


type alias Id =
    Type


toId : Int -> Id
toId id =
    Type.Var id


generate : IdGenerator expr -> Result TypeError ( expr, Id )
generate gen =
    generateWith 0 (Dict.Any.empty Common.varNameToString) gen
        |> Result.map .expr


generateWith :
    Int
    -> VarIds
    -> IdGenerator expr
    -> Output ( expr, Id )
generateWith unusedId varIds gen =
    -- TODO think of how to not depend on the VarName?
    -- TODO maybe do the ID incrementing here after the `case...of`, instead of in all the case functions?
    case gen of
        Fresh expr ->
            generateFresh unusedId varIds expr

        FreshWith1AndVar constructor gen1 name ->
            generateFreshWith1AndVar unusedId varIds constructor gen1 name

        FreshWith2 constructor gen1 gen2 ->
            generateFreshWith2 unusedId varIds constructor gen1 gen2

        FreshWith3 constructor gen1 gen2 gen3 ->
            generateFreshWith3 unusedId varIds constructor gen1 gen2 gen3

        RememberVar name gen1 ->
            generateRememberVar unusedId varIds name gen1

        FreshWith1AndMultiple constructor gens gen1 ->
            generateFreshWith1AndMultiple unusedId varIds constructor gens gen1


generateFresh : Int -> VarIds -> expr -> Output ( expr, Id )
generateFresh unusedId varIds expr =
    Ok
        { expr = ( expr, toId unusedId )
        , exprRawId = unusedId
        , unusedId = unusedId + 1
        , varIds = varIds
        }


generateFreshWith1AndMultiple : Int -> VarIds -> (List ( expr, Id ) -> ( expr, Id ) -> expr) -> List (IdGenerator expr) -> IdGenerator expr -> Output ( expr, Id )
generateFreshWith1AndMultiple unusedId0 varIds0 constructor gens gen1 =
    -- TODO make the callback hell nicer?
    generateWith unusedId0 varIds0 gen1
        |> andThen
            (\unusedId1 varIds1 bodyExpr rawId1 ->
                gens
                    |> List.foldl
                        (\gen output ->
                            output
                                |> andThen
                                    (\unusedId2 varIds2 exprsAcc _ ->
                                        generateWith unusedId2 varIds2 gen
                                            |> andThen
                                                (\unusedId3 varIds3 newExpr rawId3 ->
                                                    Ok
                                                        { unusedId = unusedId3
                                                        , varIds = varIds3
                                                        , expr = newExpr :: exprsAcc
                                                        , exprRawId = rawId3
                                                        }
                                                )
                                    )
                        )
                        (Ok
                            { unusedId = unusedId1
                            , varIds = varIds1
                            , expr = []
                            , exprRawId = rawId1
                            }
                        )
                    |> andThen
                        (\unusedId4 varIds4 exprs rawId4 ->
                            Ok
                                { expr =
                                    ( constructor exprs bodyExpr
                                    , toId unusedId4
                                    )
                                , exprRawId = rawId4
                                , unusedId = unusedId4 + 1
                                , varIds = varIds4
                                }
                        )
            )


generateFreshWith1AndVar : Int -> VarIds -> (( expr, Id ) -> Int -> expr) -> IdGenerator expr -> VarName -> Output ( expr, Id )
generateFreshWith1AndVar unusedId0 varIds0 constructor gen1 name =
    -- TODO make the callback hell nicer?
    -- TODO also this is veeery specific
    generateWith unusedId0 varIds0 gen1
        |> andThen
            (\unusedId1 varIds1 expr1 rawId1 ->
                Dict.Any.get name varIds1
                    |> Result.fromMaybe (UnknownName name)
                    |> Result.map
                        (\nameId ->
                            { expr =
                                ( constructor expr1 nameId
                                , toId unusedId1
                                )
                            , exprRawId = unusedId1
                            , unusedId = unusedId1 + 1
                            , varIds = varIds1
                            }
                        )
            )


generateFreshWith2 : Int -> VarIds -> (( expr, Id ) -> ( expr, Id ) -> expr) -> IdGenerator expr -> IdGenerator expr -> Output ( expr, Id )
generateFreshWith2 unusedId0 varIds0 constructor gen1 gen2 =
    -- TODO make the callback hell nicer?
    generateWith unusedId0 varIds0 gen1
        |> andThen
            (\unusedId1 varIds1 expr1 rawId1 ->
                generateWith unusedId1 varIds1 gen2
                    |> andThen
                        (\unusedId2 varIds2 expr2 rawId2 ->
                            Ok
                                { expr =
                                    ( constructor expr1 expr2
                                    , toId unusedId2
                                    )
                                , exprRawId = unusedId2
                                , unusedId = unusedId2 + 1
                                , varIds = varIds2
                                }
                        )
            )


generateFreshWith3 : Int -> VarIds -> (( expr, Id ) -> ( expr, Id ) -> ( expr, Id ) -> expr) -> IdGenerator expr -> IdGenerator expr -> IdGenerator expr -> Output ( expr, Id )
generateFreshWith3 unusedId0 varIds0 constructor gen1 gen2 gen3 =
    -- TODO make the callback hell nicer?
    generateWith unusedId0 varIds0 gen1
        |> andThen
            (\unusedId1 varIds1 expr1 rawId1 ->
                generateWith unusedId1 varIds1 gen2
                    |> andThen
                        (\unusedId2 varIds2 expr2 rawId2 ->
                            generateWith unusedId2 varIds2 gen3
                                |> andThen
                                    (\unusedId3 varIds3 expr3 rawId3 ->
                                        Ok
                                            { expr =
                                                ( constructor expr1 expr2 expr3
                                                , toId unusedId3
                                                )
                                            , exprRawId = unusedId3
                                            , unusedId = unusedId3 + 1
                                            , varIds = varIds3
                                            }
                                    )
                        )
            )


generateRememberVar : Int -> VarIds -> VarName -> IdGenerator expr -> Output ( expr, Id )
generateRememberVar unusedId0 varIds0 name gen =
    generateWith unusedId0 varIds0 gen
        |> map
            (\output ->
                { output | varIds = Dict.Any.insert name output.exprRawId output.varIds }
            )


map : (OutputData a -> OutputData b) -> Output a -> Output b
map fn output =
    -- TODO for some reason this doesn't work well with the typesystem sometimes (so we use andThen)
    Result.map fn output


andThen : (Int -> VarIds -> a -> Int -> Output b) -> Output a -> Output b
andThen fn output =
    Result.andThen
        (\output_ ->
            fn
                output_.unusedId
                output_.varIds
                output_.expr
                output_.exprRawId
        )
        output


fresh : expr -> IdGenerator expr
fresh =
    Fresh


freshWith1AndVar : (( expr, Id ) -> Int -> expr) -> IdGenerator expr -> VarName -> IdGenerator expr
freshWith1AndVar =
    FreshWith1AndVar


freshWith1AndMultiple : (List ( expr, Id ) -> ( expr, Id ) -> expr) -> List (IdGenerator expr) -> IdGenerator expr -> IdGenerator expr
freshWith1AndMultiple =
    FreshWith1AndMultiple


freshWith2 : (( expr, Id ) -> ( expr, Id ) -> expr) -> IdGenerator expr -> IdGenerator expr -> IdGenerator expr
freshWith2 =
    FreshWith2


freshWith3 : (( expr, Id ) -> ( expr, Id ) -> ( expr, Id ) -> expr) -> IdGenerator expr -> IdGenerator expr -> IdGenerator expr -> IdGenerator expr
freshWith3 =
    FreshWith3


rememberVar : VarName -> IdGenerator expr -> IdGenerator expr
rememberVar =
    RememberVar
