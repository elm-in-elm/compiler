module Stage.InferTypes exposing (inferExpr, inferTypes)

import AST.Canonical as Canonical
import AST.Common.Located as Located
import AST.Common.Type exposing (Type(..))
import AST.Typed as Typed
import Data.Project exposing (Project)
import Error exposing (Error(..), TypeError(..))
import Stage.InferTypes.AssignIds as AssignIds
import Stage.InferTypes.Boilerplate as Boilerplate
import Stage.InferTypes.GenerateEquations as GenerateEquations
import Stage.InferTypes.SubstitutionMap as SubstitutionMap exposing (SubstitutionMap)
import Stage.InferTypes.Unify as Unify


{-| We're using Hindley-Milner algorithm (Algorithm W). It has essentially
three parts:

  - `assignIds`: Annotate all the subexpressions with IDs
  - `generateEquations`: Generate equations (constraints) between the IDs
    (apply type inference rules)
  - `unifyAllEquations`: Solve the equations
    (find something called "most general unifier" for a specific ID)

We also have a fourth part:

  - `substituteAllInExpr` and `substituteAllInError`: recursively replace type
    variable IDs with their inferred types.

-}
inferTypes : Project Canonical.ProjectFields -> Result Error (Project Typed.ProjectFields)
inferTypes project =
    Boilerplate.inferProject inferExpr project
        |> Result.mapError TypeError


inferExpr : Canonical.LocatedExpr -> Result TypeError Typed.LocatedExpr
inferExpr located =
    let
        ( exprWithIds, idSource ) =
            AssignIds.assignIds located

        typeEquations =
            GenerateEquations.generateEquations idSource exprWithIds
                {- We throw away the IdSource. It shouldn't be needed anymore!

                   BTW GenerateEquations needed it because in case of some Exprs
                   (like List) it needed to create a new type ID on the fly.

                   TODO those IDs generated on the fly have bad visibility (you
                   can only infer their existence from the equations, but can't
                   clearly see what they belong to). Can we do something about it?
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
        substitutionMap : Result ( TypeError, SubstitutionMap ) SubstitutionMap
        substitutionMap =
            Unify.unifyAllEquations typeEquations
    in
    substitutionMap
        |> Result.map (substituteAllInExpr exprWithIds)
        |> Result.mapError substituteAllInError


{-| This function takes care of recursively applying `substituteType`
from the bottom up.
-}
substituteAllInExpr : Typed.LocatedExpr -> SubstitutionMap -> Typed.LocatedExpr
substituteAllInExpr located substitutionMap =
    Typed.transformOnce
        (substituteType substitutionMap)
        located


{-| Use whatever information you gathered before the unification failed
to make the resulting error a bit nicer.

Ie. if we have `t0 == Int` and error `List t0 /= Int`, we can do a bit better
and return `List Int /= Int` to the user.

-}
substituteAllInError : ( TypeError, SubstitutionMap ) -> TypeError
substituteAllInError ( error, substitutionMap ) =
    case error of
        TypeMismatch t1 t2 ->
            TypeMismatch
                (getBetterType substitutionMap t1)
                (getBetterType substitutionMap t2)

        OccursCheckFailed id type_ ->
            OccursCheckFailed id (getBetterType substitutionMap type_)


{-| Only care about this level, don't recurse
-}
substituteType : SubstitutionMap -> Typed.LocatedExpr -> Typed.LocatedExpr
substituteType substitutionMap located =
    Located.map
        (\( expr, type_ ) ->
            ( expr, getBetterType substitutionMap type_ )
        )
        located


{-| Tries to resolve `Var 0`-like references through the SubstitutionMap.

Only goes one step, but that should be enough if we created the SubstitutionMap
correctly. (TODO check that assumption)

Remember to call itself recursively on children Exprs!

-}
getBetterType : SubstitutionMap -> Type -> Type
getBetterType substitutionMap type_ =
    if SubstitutionMap.isEmpty substitutionMap then
        type_

    else
        case type_ of
            Int ->
                type_

            Float ->
                type_

            Char ->
                type_

            String ->
                type_

            Bool ->
                type_

            Var id ->
                -- walk one extra level
                SubstitutionMap.get id substitutionMap
                    |> Maybe.map (\typeForId -> getBetterType substitutionMap typeForId)
                    |> Maybe.withDefault type_

            Function arg result ->
                Function
                    (getBetterType substitutionMap arg)
                    (getBetterType substitutionMap result)

            List param ->
                List <| getBetterType substitutionMap param

            Unit ->
                type_

            Tuple e1 e2 ->
                Tuple
                    (getBetterType substitutionMap e1)
                    (getBetterType substitutionMap e2)

            Tuple3 e1 e2 e3 ->
                Tuple3
                    (getBetterType substitutionMap e1)
                    (getBetterType substitutionMap e2)
                    (getBetterType substitutionMap e3)

            UserDefinedType name params ->
                UserDefinedType
                    name
                    (List.map (getBetterType substitutionMap) params)
