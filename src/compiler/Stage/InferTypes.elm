module Stage.InferTypes exposing (inferExpr, inferTypes)

import AST.Canonical as Canonical
import AST.Common.Literal as Literal
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Common
import Common.Types exposing (Project)
import Dict.Any
import Error exposing (Error(..), TypeError(..))
import Stage.InferTypes.AssignIds as AssignIds
import Stage.InferTypes.Boilerplate as Boilerplate
import Stage.InferTypes.GenerateEquations as GenerateEquations
import Stage.InferTypes.IdSource as IdSource exposing (IdSource)
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
        ( exprWithIds, idSource ) =
            AssignIds.assignIds expr

        typeEquations =
            GenerateEquations.generateEquations idSource exprWithIds
                {- We throw away the IdSource. It shouldn't be needed anymore!

                   BTW GenerateEquations needed it because in case of some Exprs
                   (like List) it needed to create a new type ID on the fly.
                -}
                |> Tuple.first

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
            Unify.unifyAllEquations typeEquations
    in
    Result.map
        (substituteAllTypes exprWithIds)
        substitutionMap


{-| This function takes care of recursively applying `substituteType`
from the bottom up.
-}
substituteAllTypes : Typed.Expr -> SubstitutionMap -> Typed.Expr
substituteAllTypes expr substitutionMap =
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

            Type.List listType ->
                listType

            Type.Unit ->
                type_
