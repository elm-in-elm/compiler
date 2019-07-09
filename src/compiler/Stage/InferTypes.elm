module Stage.InferTypes exposing (inferTypes)

import AST.Canonical as Canonical
import AST.Common.Literal as Literal
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Common.Types exposing (Project)
import Dict.Any
import Error exposing (Error(..), TypeError(..))
import Stage.InferTypes.AssignIds as AssignIds
import Stage.InferTypes.Boilerplate as Boilerplate
import Stage.InferTypes.IdSource as Id
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

Gives every subexpression an unique auto-incremented ID and converts
from Canonical.Expr to Typed.Expr.

Example:

Input (forgive the nonsense AST):

    Canonical.If
        { test =
            Canonical.Plus
                (Canonical.Literal (Int 1))
                (Canonical.Literal (Int 2))
        , then_ = Canonical.Unit
        , else_ = Canonical.Literal (Bool True)
        }

Output:

    ( Typed.If
        { test =
            ( Typed.Plus
                ( Typed.Literal (Int 1)
                , Var 0
                )
                ( Typed.Literal (Int 2)
                , Var 1
                )
            , Var 2
            )
        , then_ = ( Typed.Unit, Var 3 )
        , else_ = ( Typed.Literal (Bool True), Var 4 )
        }
    , Var 5
    )

-}
assignIds : Canonical.Expr -> Result TypeError Typed.Expr
assignIds expr =
    expr
        |> AssignIds.toIdGenerator
        |> Id.generate


{-| Stage 2

We try to bind various subexpressions together here.
For example consider this:

    42

This is an Integer (or a `number`, we don't care here for the sake of simplicity).

    foo 42

What can we say about `foo`? It's a function from Integer (as evidenced by 42),
but we don't know to what. Right now the best we could say is `foo : Int -> a`.

    foo 42 == "abc"

Suddenly everything's clear: the whole thing is Bool because of `==`, and thanks
to the right side of `==` being a String and the two sides having to match in types,
we know `foo 42` is a String too, and thus `foo : Int -> String`.

The "two sides of `==` have to match" insight could be expressed here as something like

    equals leftType rightType

-}
generateEquations : Typed.Expr -> List TypeEquation
generateEquations ( expr, type_ ) =
    case expr of
        Typed.Literal (Literal.Int _) ->
            -- integer is an integer ¯\_(ツ)_/¯
            [ equals type_ Type.Int ]

        Typed.Literal (Literal.Float _) ->
            -- float is a float
            [ equals type_ Type.Float ]

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

        Typed.List list ->
            case type_ of
                Type.List type1 ->
                    List.foldl
                        (\(entry, type2) acc ->
                            (equals type1 type2) :: acc
                        )
                        []
                        list
                
                _ ->
                    []

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

            Type.Float ->
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

            Type.List list ->
                list

            Type.Unit ->
                type_
