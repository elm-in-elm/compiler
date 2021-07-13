module Stage.InferTypes exposing (inferExpr, inferTypes, unifyWithTypeAnnotation)

{-| The intent is to have all the interesting stuff in here, and all the boring
mamping/wrapping code in `Stage.InferTypes.Boilerplate`.
-}

import Dict exposing (Dict)
import Elm.AST.Canonical as Canonical
import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (Error(..), TypeError(..))
import Elm.Data.Declaration
    exposing
        ( Declaration
        , DeclarationBody(..)
        )
import Elm.Data.Located as Located
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type exposing (Id, Type(..), TypeOrId(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import Stage.InferTypes.AssignIds as AssignIds
import Stage.InferTypes.Boilerplate as Boilerplate
import Stage.InferTypes.Environment exposing (Environment)
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
        |> Result.mapError TypeError


inferExpr :
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Canonical.LocatedExpr
    -> Result ( TypeError, SubstitutionMap ) ( Typed.LocatedExpr, ( SubstitutionMap, Id, Environment ) )
inferExpr aliases unusedId env substitutionMap located =
    let
        ( exprWithIds, unusedId1 ) =
            AssignIds.assignIds unusedId located

        ( unusedId2, env1, localTypeEquations ) =
            GenerateEquations.generateLocalEquations unusedId1 env exprWithIds

        typeEquationsAcrossDeclarations =
            {- TODO this should be ran only once per project, else there will be
               a bunch of duplicates I'm afraid.
            -}
            GenerateEquations.generateEquationsAcrossDeclarations env1

        allTypeEquations =
            -- use them both
            localTypeEquations ++ typeEquationsAcrossDeclarations

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
            Unify.unifyAllEquations allTypeEquations aliases substitutionMap
    in
    newSubstitutionMap
        |> Result.map
            (\map ->
                let
                    substituted =
                        substituteAllInExpr exprWithIds map
                in
                ( substituted, ( map, unusedId2, env1 ) )
            )
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

                    TypeVar _ ->
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
    Dict ( ModuleName, VarName ) (ConcreteType Qualified)
    -> Id
    -> Environment
    -> SubstitutionMap
    -> Declaration Typed.LocatedExpr (ConcreteType Qualified) Qualified
    -> Result ( TypeError, SubstitutionMap ) ( Declaration Typed.LocatedExpr Never Qualified, ( SubstitutionMap, Id, Environment ) )
unifyWithTypeAnnotation aliases unusedId env substitutionMap decl =
    let
        default =
            Ok
                ( throwAwayType decl
                , ( substitutionMap
                  , unusedId
                  , env
                  )
                )
    in
    case decl.body of
        Value r ->
            case r.typeAnnotation of
                Just annotationType ->
                    let
                        realDeclarationType =
                            Typed.getTypeOrId r.expression

                        unifyResult =
                            Unify.unify
                                (ConcreteType.toTypeOrId annotationType)
                                realDeclarationType
                                aliases
                                substitutionMap
                    in
                    unifyResult
                        |> Result.map
                            (\newSubstitutionMap ->
                                ( throwAwayType decl
                                , ( newSubstitutionMap
                                  , unusedId
                                  , env
                                  )
                                )
                            )

                Nothing ->
                    default

        TypeAlias _ ->
            default

        CustomType _ ->
            default

        Port _ ->
            default


throwAwayType : Declaration a (ConcreteType Qualified) b -> Declaration a Never b
throwAwayType decl =
    { module_ = decl.module_
    , name = decl.name
    , body =
        case decl.body of
            Value r ->
                Value
                    { expression = r.expression
                    , typeAnnotation = Nothing
                    }

            TypeAlias r ->
                TypeAlias
                    { parameters = r.parameters
                    , definition = r.definition
                    }

            CustomType r ->
                CustomType
                    { parameters = r.parameters
                    , constructors = r.constructors
                    }

            Port type_ ->
                Port type_
    }
