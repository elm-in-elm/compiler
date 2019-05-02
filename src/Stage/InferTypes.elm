module Stage.InferTypes exposing (inferTypes)

import AST.Canonical as Canonical
import AST.Common.Literal as Literal
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Common
import Common.Types
    exposing
        ( Dict_
        , Module
        , Modules
        , Project
        , TopLevelDeclaration
        , VarName
        )
import Dict exposing (Dict)
import Dict.Any exposing (AnyDict)
import Error exposing (Error(..), TypeError(..))
import Extra.Dict.Any
import Extra.Tuple
import Stage.InferTypes.Boilerplate as Boilerplate
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Stage.InferTypes.TypeEquation exposing (TypeEquation, equals)
import Stage.InferTypes.Unify as Unify


{-| We're using Hindley-Milner algorithm (Algorithm W). It has essentially
three parts:

  - `assignIds`: Annotate all the subexpressions with IDs
  - `generateEquations`: Generate equations (constraints) between the IDs
    (apply type inference rules)
  - `unifyAllEquations`: Solve the equations
    (find something called "most general unifier" for a specific ID)

We also have a fourth part:

  - `substituteAllTypes`: recursively replace type variable IDs with their
    inferred types.

TODO this can't work, we'll have to typecheck across modules, right?
This only typechecks each module separately...

-}
inferTypes : Project Canonical.ProjectFields -> Result Error (Project Typed.ProjectFields)
inferTypes project =
    Boilerplate.inferProject inferExpr project
        |> Result.mapError TypeError


inferExpr : Canonical.Expr -> Result TypeError Typed.Expr
inferExpr expr =
    let
        exprWithIds : Result TypeError Typed.Expr
        exprWithIds =
            assignIds expr

        {- We have an interesting dilemma:

           Should we carry the substitution map around, keep the typed expr
           consisting of type variable IDs and primitive types and substitute for
           the inferred types at the last possible moment (when reporting errors)?

           Or should we substitute all possible type variable IDs before
           returning from this function, and ditch the substitution map?

           The second option seems like an unnecessary work, but for the purposes
           of readability and education we go with it.
        -}
        substitutionMap : Result TypeError SubstitutionMap
        substitutionMap =
            Result.andThen
                (generateEquations >> Unify.unifyAllEquations)
                exprWithIds
    in
    Result.map2 substituteAllTypes
        substitutionMap
        exprWithIds



-- TODO think about which functions should live in which modules.


{-| Stage 1

TODO document better

-}
assignIds : Canonical.Expr -> Result TypeError Typed.Expr
assignIds expr =
    assignIdsHelp 0 (Dict.Any.empty Common.varNameToString) expr
        |> Result.map Extra.Tuple.third


assignIdsHelp : Int -> AnyDict String VarName Int -> Canonical.Expr -> Result TypeError ( Int, AnyDict String VarName Int, Typed.Expr )
assignIdsHelp unusedId0 varIds0 expr =
    {- TODO is there a nicer, maybe monadic, way to do this, instead of
       threading this state ourselves?
    -}
    (case expr of
        {- With literals, we could plug their final type in right here
           (no solving needed!) but let's be uniform and do everything through
           the constraint solver in stages 2 and 3.
        -}
        Canonical.Literal literal ->
            Ok
                ( unusedId0
                , varIds0
                , Typed.Literal literal
                )

        -- We remember argument's IDs so that we can later use them in Lambda
        Canonical.Argument name ->
            Ok
                ( unusedId0
                , Dict.Any.insert name unusedId0 varIds0
                , Typed.Argument name
                )

        Canonical.Var name ->
            Ok
                ( unusedId0
                  -- TODO is this right?
                , varIds0
                , Typed.Var name
                )

        Canonical.Plus e1 e2 ->
            {- TODO elm-format makes this look ugly ... is there a different way?
               What's hurting the readability is essentially the lack of do notation...
               similarly to how List.Extra.andThen usages aren't very pretty.
            -}
            assignIdsHelp unusedId0 varIds0 e1
                |> Result.andThen
                    (\( unusedId1, varIds1, e1_ ) ->
                        assignIdsHelp unusedId1 varIds1 e2
                            |> Result.map
                                (\( unusedId2, varIds2, e2_ ) ->
                                    ( unusedId2
                                    , varIds2
                                    , Typed.Plus e1_ e2_
                                    )
                                )
                    )

        Canonical.Lambda { argument, body } ->
            assignIdsHelp unusedId0 varIds0 body
                |> Result.andThen
                    (\( unusedId1, varIds1, body_ ) ->
                        Dict.Any.get argument varIds1
                            |> Result.fromMaybe (UnknownName argument)
                            |> Result.map
                                (\argumentId ->
                                    ( unusedId1
                                    , varIds1
                                    , Typed.lambda argument body_ argumentId
                                    )
                                )
                    )

        Canonical.Call { fn, argument } ->
            assignIdsHelp unusedId0 varIds0 fn
                |> Result.andThen
                    (\( unusedId1, varIds1, fn_ ) ->
                        assignIdsHelp unusedId1 varIds1 argument
                            |> Result.map
                                (\( unusedId2, varIds2, argument_ ) ->
                                    ( unusedId2
                                    , varIds2
                                    , Typed.Call
                                        { fn = fn_
                                        , argument = argument_
                                        }
                                    )
                                )
                    )

        Canonical.If { test, then_, else_ } ->
            assignIdsHelp unusedId0 varIds0 test
                |> Result.andThen
                    (\( unusedId1, varIds1, test_ ) ->
                        assignIdsHelp unusedId1 varIds1 then_
                            |> Result.andThen
                                (\( unusedId2, varIds2, then__ ) ->
                                    assignIdsHelp unusedId2 varIds2 else_
                                        |> Result.map
                                            (\( unusedId3, varIds3, else__ ) ->
                                                ( unusedId3
                                                , varIds3
                                                , Typed.If
                                                    { test = test_
                                                    , then_ = then__
                                                    , else_ = else__
                                                    }
                                                )
                                            )
                                )
                    )
    )
        |> Result.map
            (\( unusedIdN, varIdsN, recursedExpr ) ->
                ( unusedIdN + 1
                , varIdsN
                , ( recursedExpr, Type.Var unusedIdN )
                )
            )


{-| Stage 2

TODO document better

-}
generateEquations : Typed.Expr -> List TypeEquation
generateEquations ( expr, type_ ) =
    case expr of
        Typed.Literal (Literal.Int _) ->
            -- integer is an integer ¯\_(ツ)_/¯
            [ equals type_ Type.Int ]

        Typed.Literal (Literal.Char _) ->
            -- char is a char
            [ equals type_ Type.Char ]

        Typed.Literal (Literal.String _) ->
            -- string is a string
            [ equals type_ Type.String ]

        Typed.Literal (Literal.Bool _) ->
            -- bool is a bool
            [ equals type_ Type.Bool ]

        Typed.Argument _ ->
            -- we can't make any assumptions here
            []

        Typed.Var _ ->
            -- we can't make any assumptions here
            []

        Typed.Plus left right ->
            let
                ( _, leftType ) =
                    left

                ( _, rightType ) =
                    right
            in
            -- for expression `a + b`:
            [ equals leftType Type.Int -- type of `a` is Int
            , equals rightType Type.Int -- type of `b` is Int
            , equals type_ Type.Int -- type of `a + b` is Int
            ]
                -- (don't forget `a` and `b` can be arbitrary expressions!)
                ++ generateEquations left
                ++ generateEquations right

        Typed.Lambda { body, argumentId } ->
            let
                ( _, bodyType ) =
                    body
            in
            -- type of `\arg -> body` is (arg -> body)
            equals type_ (Type.Function (Type.Var argumentId) bodyType)
                -- (don't forget `body` can be arbitrary expression!)
                :: generateEquations body

        Typed.Call { fn, argument } ->
            let
                ( _, fnType ) =
                    fn

                ( _, argumentType ) =
                    argument
            in
            -- for expression `a b`:
            -- type of `a` is (argumentType -> resultType)
            equals fnType (Type.Function argumentType type_)
                -- (don't forget `a` and `b` can be arbitrary expressions!)
                :: generateEquations fn
                ++ generateEquations argument

        Typed.If { test, then_, else_ } ->
            let
                ( _, testType ) =
                    test

                ( _, thenType ) =
                    then_

                ( _, elseType ) =
                    else_
            in
            -- for expression `if a then b else c`:
            [ equals testType Type.Bool -- type of `a` is Bool

            --, equals thenType TODO -- type of `b` is unknkown -- TODO remove this if unneeded
            , equals thenType elseType -- types of `b` and `c` are the same
            , equals thenType type_ -- types of `b` and `if a then b else c` are the same
            ]
                -- (don't forget `a`, `b` and `c` can be arbitrary expressions!)
                ++ generateEquations test
                ++ generateEquations then_
                ++ generateEquations else_


{-| This function takes care of recursively applying `substituteType`
from the bottom up.
-}
substituteAllTypes : SubstitutionMap -> Typed.Expr -> Typed.Expr
substituteAllTypes substitutionMap expr =
    Typed.transformOnce
        (substituteType substitutionMap)
        expr


{-| Only care about this level, don't recurse
-}
substituteType : SubstitutionMap -> Typed.Expr -> Typed.Expr
substituteType substitutionMap ( expr, type_ ) =
    ( expr, getType substitutionMap type_ )


getType : SubstitutionMap -> Type -> Type
getType substitutionMap type_ =
    -- TODO rename types afterwards? (t0,t1,t2 -> a,b,c)
    if SubstitutionMap.isEmpty substitutionMap then
        type_

    else
        case type_ of
            Type.Int ->
                type_

            Type.Char ->
                type_

            Type.String ->
                type_

            Type.Bool ->
                type_

            Type.Var id ->
                SubstitutionMap.get id substitutionMap
                    |> Maybe.map (\typeForId -> getType substitutionMap typeForId)
                    |> Maybe.withDefault type_

            Type.Function arg result ->
                Type.Function
                    (getType substitutionMap arg)
                    (getType substitutionMap result)
