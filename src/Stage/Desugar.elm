module Stage.Desugar exposing (desugar)

import AST.Canonical as Canonical
import AST.Frontend as Frontend
import Basics.Extra exposing (flip)
import Common
import Common.Types
    exposing
        ( Dict_
        , Module
        , ModuleName
        , Modules
        , Project
        , TopLevelDeclaration
        , VarName
        )
import Dict.Any exposing (AnyDict)
import Error exposing (DesugarError(..), Error(..))
import Extra.Dict.Any
import Maybe.Extra


desugar : Project Frontend.ProjectFields -> Result Error (Project Canonical.ProjectFields)
desugar p =
    p.modules
        |> resultMapDict Common.moduleNameToString (desugarModule p.modules)
        |> Result.map
            (\modules ->
                { elmJson = p.elmJson
                , mainFilePath = p.mainFilePath
                , mainModuleName = p.mainModuleName
                , sourceDirectory = p.sourceDirectory
                , modules = modules
                }
            )
        |> Result.mapError DesugarError


desugarModule : Modules Frontend.Expr -> Module Frontend.Expr -> Result DesugarError (Module Canonical.Expr)
desugarModule modules thisModule =
    thisModule.topLevelDeclarations
        |> resultMapDict Common.varNameToString (desugarTopLevelDeclaration modules thisModule)
        |> Result.map
            (\topLevelDeclarations ->
                { dependencies = thisModule.dependencies
                , name = thisModule.name
                , filePath = thisModule.filePath
                , type_ = thisModule.type_
                , exposing_ = thisModule.exposing_
                , topLevelDeclarations = topLevelDeclarations
                }
            )


{-| Roughly: Dict.Any.map toResult >> Result.Extra.combine
We might need to make the function accept keys if there arises a need for it.
-}
resultMapDict : (k -> comparable) -> (v -> Result x v2) -> AnyDict comparable k v -> Result x (AnyDict comparable k v2)
resultMapDict toComparable fn dict =
    dict
        |> Dict.Any.toList
        -- This following line is a mouthful. It uses the Result-producing fn on the second part of the tuple,
        -- and in the same loop does what Result.Extra.combine would did with that second part of the tuple:
        -- List (a, Result x b) -> Result x (List (a,b))
        |> List.foldr (\( a, b ) acc -> Result.map2 (\b_ acc_ -> ( a, b_ ) :: acc_) (fn b) acc) (Ok [])
        |> Result.map (Dict.Any.fromList toComparable)


desugarTopLevelDeclaration : Modules Frontend.Expr -> Module Frontend.Expr -> TopLevelDeclaration Frontend.Expr -> Result DesugarError (TopLevelDeclaration Canonical.Expr)
desugarTopLevelDeclaration modules thisModule decl =
    desugarExpr modules thisModule decl
        |> Result.map
            (\body ->
                { name = decl.name
                , module_ = decl.module_
                , body = body
                }
            )


{-| Combines the various small desugaring passes into one pass that tries all
of them. Thus, we get some separation of concerns - each pass only cares about
a small subset of the whole process!
-}
desugarExpr : Modules Frontend.Expr -> Module Frontend.Expr -> TopLevelDeclaration Frontend.Expr -> Result DesugarError Canonical.Expr
desugarExpr modules thisModule decl =
    let
        expr =
            decl.body
    in
    desugarWithPasses
        [ desugarLiteral
        , desugarVar modules thisModule
        , desugarArgument
        , desugarPlus
        , desugarLambda
        ]
        expr
        {- TODO there may be a way to return the failing expr instead of the
           top-level one... we're doing this Result.fromMaybe in a wrong spot.

           That will mean the `desugarWithPasses` function will have to know
           about the Result DesugarError stuff and not just deal with Maybes.
           Is it worth it? (Probably yes but we'll have to try and see.)
        -}
        |> Result.fromMaybe (NoDesugarPass decl)
        |> Result.andThen identity


{-| The `Maybe` decides whether we touch that Expr constructor or not (just so
that we can combine the many desugar passes together).

The `Result` is then a legitimate case of us trying to desugar and failing.
See `DesugarError` for what everything can go wrong.

The function in the first argument is a way to recurse into child Exprs.

-}
type alias DesugarPass =
    (Frontend.Expr -> Maybe (Result DesugarError Canonical.Expr))
    -> Frontend.Expr
    -> Maybe (Result DesugarError Canonical.Expr)


{-| TODO Think about putting this into the Transform library.
-}
desugarWithPasses : List ((a -> Maybe b) -> (a -> Maybe b)) -> a -> Maybe b
desugarWithPasses passes expr =
    let
        recurse : a -> Maybe b
        recurse expr_ =
            -- TODO maybe we can be more efficient with (not) repeating work
            desugarWithPasses passes expr_

        {- Just give the recursion argument to the pass.

           We could do some cleverness with something like `((<|) recurse)`
           instead of this function, but I think having it explicitly written
           (and with all the types!) is better.

        -}
        supplyRecursion : ((a -> Maybe b) -> (a -> Maybe b)) -> (a -> Maybe b)
        supplyRecursion pass =
            pass recurse

        multipass : a -> Maybe b
        multipass =
            orList (List.map supplyRecursion passes)
    in
    multipass expr


or : (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
or leftFn rightFn value =
    let
        left =
            leftFn value
    in
    if left == Nothing then
        rightFn value

    else
        left


orList : List (a -> Maybe b) -> (a -> Maybe b)
orList fns =
    List.foldl or (always Nothing) fns



-- DESUGAR PASSES


desugarLiteral : DesugarPass
desugarLiteral _ expr =
    case expr of
        Frontend.Literal literal ->
            Just (Ok (Canonical.Literal literal))

        _ ->
            Nothing


desugarVar : Modules Frontend.Expr -> Module Frontend.Expr -> DesugarPass
desugarVar modules thisModule _ expr =
    case expr of
        Frontend.Var ( maybeModuleName, varName ) ->
            findModuleOfVar modules thisModule maybeModuleName varName
                |> Result.fromMaybe (VarNotInEnvOfModule ( maybeModuleName, varName ) thisModule.name)
                |> Result.map (\moduleName -> Canonical.Var ( moduleName, varName ))
                |> Just

        _ ->
            Nothing


desugarArgument : DesugarPass
desugarArgument _ expr =
    case expr of
        Frontend.Argument varName ->
            Just (Ok (Canonical.Argument varName))

        _ ->
            Nothing


desugarPlus : DesugarPass
desugarPlus recurse expr =
    case expr of
        Frontend.Plus e1 e2 ->
            Maybe.map2 (Result.map2 Canonical.Plus)
                (recurse e1)
                (recurse e2)

        _ ->
            Nothing


desugarLambda : DesugarPass
desugarLambda recurse expr =
    case expr of
        Frontend.Lambda { arguments, body } ->
            recurse body
                |> Maybe.map (Result.map (curryLambda arguments))

        _ ->
            Nothing



-- HELPERS


{-| Convert a multi-arg lambda into multiple single-arg lambdas.

    Frontend.Lambda [ arg1, arg2 ] body

    -->
    Canonical.Lambda arg1 (Canonical.Lambda arg2 body)

-}
curryLambda : List VarName -> Canonical.Expr -> Canonical.Expr
curryLambda arguments body =
    List.foldr
        (\argument body_ ->
            Canonical.Lambda
                { argument = argument
                , body = body_
                }
        )
        body
        arguments


{-| We have roughly these options:

  - bar = >baz< (baz being defined elsewhere in this module)
  - import Foo exposing (baz); bar = >baz<
  - import Foo; bar = >Foo.baz<
  - import Foo as F; bar = >F.baz<

In all these cases we need to find the full unaliased module name of the var.

-}
findModuleOfVar : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
findModuleOfVar modules thisModule maybeModuleName varName =
    -- TODO test all these
    {- TODO does this allow for some collisions by "returning early"?
       Should we check that exactly one is Just and the others are Nothing?
    -}
    unqualifiedVarInThisModule thisModule maybeModuleName varName
        |> Maybe.Extra.orElseLazy (\() -> unqualifiedVarInImportedModule modules thisModule maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInImportedModule modules maybeModuleName varName)
        |> Maybe.Extra.orElseLazy (\() -> qualifiedVarInAliasedModule modules thisModule maybeModuleName varName)


unqualifiedVarInThisModule : Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
unqualifiedVarInThisModule thisModule maybeModuleName varName =
    if maybeModuleName == Nothing && Dict.Any.member varName thisModule.topLevelDeclarations then
        Just thisModule.name

    else
        Nothing


unqualifiedVarInImportedModule : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
unqualifiedVarInImportedModule modules thisModule maybeModuleName varName =
    if maybeModuleName == Nothing then
        -- find a module which exposes that var
        thisModule.dependencies
            |> Extra.Dict.Any.find
                (\_ dependency ->
                    Dict.Any.get dependency.moduleName modules
                        |> Maybe.map (Common.exposes varName modules)
                        |> Maybe.withDefault False
                )
            |> Maybe.map (\( k, v ) -> v.moduleName)

    else
        Nothing


qualifiedVarInImportedModule : Modules Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
qualifiedVarInImportedModule modules maybeModuleName varName =
    maybeModuleName
        |> Maybe.andThen (flip Dict.Any.get modules)
        |> Maybe.andThen
            (\module_ ->
                if Dict.Any.member varName module_.topLevelDeclarations then
                    Just maybeModuleName

                else
                    Nothing
            )
        |> Maybe.withDefault Nothing


qualifiedVarInAliasedModule : Modules Frontend.Expr -> Module Frontend.Expr -> Maybe ModuleName -> VarName -> Maybe ModuleName
qualifiedVarInAliasedModule modules thisModule maybeModuleName varName =
    let
        unaliasedModuleName =
            Maybe.andThen (Common.unalias thisModule) maybeModuleName
    in
    -- Reusing the existing functionality. TODO is this a good idea?
    qualifiedVarInImportedModule modules unaliasedModuleName varName
