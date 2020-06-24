module Stage.InferTypes exposing (inferExpr, inferTypes, unifyWithTypeAnnotation)

import Dict
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (Error(..), TypeError(..))
import Elm.Data.Declaration as Declaration exposing (Declaration)
import Elm.Data.Located as Located
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type exposing (Type(..), TypeOrId(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
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
    project
        |> Boilerplate.inferProject
            inferExpr
            unifyWithTypeAnnotation
            SubstitutionMap.empty
        |> Result.mapError TypeError


inferExpr :
    SubstitutionMap
    -> Canonical.LocatedExpr
    -> Result ( TypeError, SubstitutionMap ) ( Typed.LocatedExpr, SubstitutionMap )
inferExpr substitutionMap located =
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
        newSubstitutionMap : Result ( TypeError, SubstitutionMap ) SubstitutionMap
        newSubstitutionMap =
            Unify.unifyAllEquations typeEquations substitutionMap
    in
    newSubstitutionMap
        |> Result.map (\map -> ( substituteAllInExpr exprWithIds map, map ))
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
substituteAllInError : ( TypeError, SubstitutionMap ) -> ( TypeError, SubstitutionMap )
substituteAllInError ( error, substitutionMap ) =
    ( case error of
        TypeMismatch t1 t2 ->
            TypeMismatch
                (getBetterType substitutionMap t1)
                (getBetterType substitutionMap t2)

        OccursCheckFailed id type_ ->
            OccursCheckFailed id (getBetterType substitutionMap type_)

        AnnotationForNonExprDeclaration ->
            AnnotationForNonExprDeclaration
    , substitutionMap
    )


{-| Only care about this level, don't recurse. The recursion is handled by
`transform` library.
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
correctly. (Any references not fully resolved are bugs, please report them!)

Remember to call itself recursively on children Exprs!

-}
getBetterType : SubstitutionMap -> TypeOrId Qualified -> TypeOrId Qualified
getBetterType substitutionMap typeOrId =
    if SubstitutionMap.isEmpty substitutionMap then
        typeOrId

    else
        case typeOrId of
            Id id ->
                -- walk one extra level
                -- TODO: why? explain
                SubstitutionMap.get id substitutionMap
                    |> Maybe.map (getBetterType substitutionMap)
                    |> Maybe.withDefault typeOrId

            Type type_ ->
                case type_ of
                    Int ->
                        typeOrId

                    Float ->
                        typeOrId

                    Char ->
                        typeOrId

                    String ->
                        typeOrId

                    Bool ->
                        typeOrId

                    Var _ ->
                        typeOrId

                    Function { from, to } ->
                        Type <|
                            Function
                                { from = getBetterType substitutionMap from
                                , to = getBetterType substitutionMap to
                                }

                    List param ->
                        Type <|
                            List <|
                                getBetterType substitutionMap param

                    Unit ->
                        typeOrId

                    Tuple e1 e2 ->
                        Type <|
                            Tuple
                                (getBetterType substitutionMap e1)
                                (getBetterType substitutionMap e2)

                    Tuple3 e1 e2 e3 ->
                        Type <|
                            Tuple3
                                (getBetterType substitutionMap e1)
                                (getBetterType substitutionMap e2)
                                (getBetterType substitutionMap e3)

                    UserDefinedType ut ->
                        Type <|
                            UserDefinedType
                                { ut | args = List.map (getBetterType substitutionMap) ut.args }

                    Record bindings ->
                        Type <|
                            Record <|
                                Dict.map (\_ binding -> getBetterType substitutionMap binding) bindings


unifyWithTypeAnnotation :
    SubstitutionMap
    -> Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified
    -> Result ( TypeError, SubstitutionMap ) ( Declaration Typed.LocatedExpr Never Qualified, SubstitutionMap )
unifyWithTypeAnnotation substitutionMap decl =
    case ( decl.body, decl.typeAnnotation ) of
        ( Declaration.Value expr, Just annotationType ) ->
            let
                realDeclarationType =
                    Typed.getTypeOrId expr

                unifyResult =
                    Unify.unify
                        (ConcreteType.toTypeOrId annotationType)
                        realDeclarationType
                        substitutionMap
            in
            unifyResult
                |> Result.map
                    (\newSubstitutionMap ->
                        ( throwAwayType decl
                        , newSubstitutionMap
                        )
                    )

        ( _, Nothing ) ->
            Ok
                ( throwAwayType decl
                , substitutionMap
                )

        ( _, Just annotationType ) ->
            Err
                ( AnnotationForNonExprDeclaration
                , substitutionMap
                )


throwAwayType : Declaration a (ConcreteType Qualified) b -> Declaration a Never b
throwAwayType decl =
    { module_ = decl.module_
    , typeAnnotation = Nothing
    , name = decl.name
    , body = decl.body
    }
