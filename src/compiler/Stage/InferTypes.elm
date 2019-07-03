module Stage.InferTypes exposing (inferTypes)

import AST.Canonical as Canonical
import AST.Common.Literal as Literal
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Basics.Extra exposing (uncurry)
import Common
import Common.Types
    exposing
        ( Project
        , VarName
        )
import Dict.Any exposing (AnyDict)
import Error exposing (Error(..), TypeError(..))
import Extra.Dict.Any
import Extra.Tuple
import Stage.InferTypes.Boilerplate as Boilerplate
import Stage.InferTypes.IdSource as Id exposing (IdGenerator)
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
                |> Debug.log "ids"

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
    expr
        |> toIdGenerator
        |> Id.generate


toIdGenerator : Canonical.Expr -> IdGenerator Typed.Expr_
toIdGenerator expr =
    case expr of
        {- With literals, we could plug their final type in right here
           (no solving needed!) but let's be uniform and do everything through
           the constraint solver in stages 2 and 3.
        -}
        Canonical.Literal literal ->
            Id.fresh (Typed.Literal literal)

        -- We remember argument's IDs so that we can later use them in Lambda
        Canonical.Argument name ->
            Id.fresh (Typed.Argument name)
                |> Id.rememberVar name

        Canonical.Var name ->
            Id.fresh (Typed.Var name)

        Canonical.Plus e1 e2 ->
            Id.freshWith2 Typed.Plus
                (toIdGenerator e1)
                (toIdGenerator e2)

        Canonical.Lambda { argument, body } ->
            Id.freshWith1AndVar (Typed.lambda argument)
                (toIdGenerator body)
                argument

        Canonical.Call { fn, argument } ->
            Id.freshWith2
                (\fn_ argument_ ->
                    Typed.Call
                        { fn = fn_
                        , argument = argument_
                        }
                )
                (toIdGenerator fn)
                (toIdGenerator argument)

        Canonical.If { test, then_, else_ } ->
            Id.freshWith3
                (\test_ then__ else__ ->
                    Typed.If
                        { test = test_
                        , then_ = then__
                        , else_ = else__
                        }
                )
                (toIdGenerator test)
                (toIdGenerator then_)
                (toIdGenerator else_)

        Canonical.Let { bindings, body } ->
            {- We don't thread the full (VarName, Binding) thing to IdSource
               as that would bloat the type signatures of IdGenerator too much.

               We unwrap the exprs from the bindings and then carefully put them
               back together in the same order (see the List.map2 below).
            -}
            let
                bindingsList =
                    Dict.Any.toList bindings
            in
            Id.freshWith1AndMultiple
                (\bindingsList_ body_ ->
                    Typed.let_
                        (Dict.Any.fromList
                            Common.varNameToString
                            (List.map2 (\( name, _ ) body__ -> ( name, { name = name, body = body__ } ))
                                bindingsList
                                bindingsList_
                            )
                        )
                        body_
                )
                (List.map (Tuple.second >> .body >> toIdGenerator) bindingsList)
                (toIdGenerator body)

        Canonical.Unit ->
            Id.fresh Typed.Unit


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
            , equals thenType elseType -- types of `b` and `c` are the same
            , equals thenType type_ -- types of `b` and `if a then b else c` are the same
            ]
                -- (don't forget `a`, `b` and `c` can be arbitrary expressions!)
                ++ generateEquations test
                ++ generateEquations then_
                ++ generateEquations else_

        Typed.Let { bindings, body } ->
            let
                ( _, bodyType ) =
                    body

                bindingEquations =
                    bindings
                        |> Dict.Any.values
                        |> List.concatMap (.body >> generateEquations)
            in
            -- for expression `let x = a, y = b in c` (pardon the comma):
            -- type of the whole let and type of `c` are the same
            equals bodyType type_
                -- (don't forget `a`, `b` and `c` can be arbitrary expressions!)
                :: generateEquations body
                ++ bindingEquations

        Typed.Unit ->
            -- unit is unit
            [ equals type_ Type.Unit ]


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

            Type.Unit ->
                type_
