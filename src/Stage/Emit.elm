module Stage.Emit exposing
    ( projectToDeclarationList
    , modulesToDeclarationLists
    )

{-| Helpers for the various Stage.Emit.<LANGUAGE> modules.

If you're looking for emit functions for a specific target language,
look into Stage.Emit.<LANGUAGE> modules.

There are two emit usecases we know of:

1.  all modules to one output file, dead code eliminated (eg. Elm to JS)

@docs projectToDeclarationList

1.  each module to its own output file, dead code eliminated (eg. Elm to Elixir?)
    The fact that we don't have `main` function(s) doesn't stop us - we can think
    of all the _exposed functions_ from all modules as potential entrypoints.

@docs modulesToDeclarationLists

There _might_ be usecases for not eliminating dead code? We don't do that
currently because we don't know of any, but if there are, please raise an issue
in the GitHub repo!

TODO we'll probably have to detect cycles and do something like IIFE

-}

import AST.Common.Type as Type exposing (Type, TypeArgument(..))
import AST.Typed as Typed exposing (Expr_(..))
import AssocList as Dict exposing (Dict)
import AssocList.Extra as Dict
import AssocSet as Set exposing (Set)
import Data.Declaration exposing (Declaration, DeclarationBody(..))
import Data.Exposing as Exposing exposing (ExposedItem(..), Exposing(..))
import Data.Module exposing (Module, Modules)
import Data.ModuleName exposing (ModuleName)
import Data.Project exposing (Project)
import Data.VarName as VarName exposing (VarName)
import Error exposing (EmitError(..))
import Graph
import Result.Extra as Result


projectToDeclarationList : Project Typed.ProjectFields -> Result EmitError (List (Declaration Typed.LocatedExpr))
projectToDeclarationList { mainModuleName, modules } =
    modulesToGraph mainModuleName modules
        |> Result.map (findPathToMain mainModuleName)


modulesToDeclarationLists : Project Typed.ProjectFields -> Result EmitError (Dict ModuleName (List (Declaration Typed.LocatedExpr)))
modulesToDeclarationLists ({ mainModuleName, modules } as project) =
    modulesToGraph mainModuleName modules
        |> Result.map (findPathForEachModule project)


type alias Graph =
    Graph.Graph (Declaration Typed.LocatedExpr) ()


{-| We want to be able to emit `main`. We only emit what's needed for that.
Taken from the example in elm-community/graph README :sweat\_smile:
-}
findPathToMain : ModuleName -> Graph -> List (Declaration Typed.LocatedExpr)
findPathToMain mainModuleName programGraph =
    findPath
        programGraph
        (Set.singleton ( mainModuleName, VarName.fromString "main" ))


{-| In this case we don't have `main`s but
-}
findPathForEachModule : Project Typed.ProjectFields -> Graph -> Dict ModuleName (List (Declaration Typed.LocatedExpr))
findPathForEachModule project graph =
    let
        exposedDeclarations : Set ( ModuleName, VarName )
        exposedDeclarations =
            project.modules
                |> Dict.values
                |> List.concatMap moduleToExposedDeclarations
                |> Set.fromList
                |> Set.map (\decl -> ( decl.module_, decl.name ))

        moduleToExposedDeclarations : Module Typed.LocatedExpr -> List (Declaration Typed.LocatedExpr)
        moduleToExposedDeclarations module_ =
            case module_.exposing_ of
                ExposingAll ->
                    Dict.values module_.declarations

                ExposingSome exposedItems ->
                    exposedItems
                        -- throwing away stuff - see comment for `exposedItemToDeclaration`
                        |> List.filterMap (exposedItemToDeclaration module_)

        {- We'll be throwing away Nothings created here - those happen if the
           compiler can't find a definition that is supposed to be exposed.

           Maybe we should return a Result here (Err (DeclarationExposedButNotDefined ...))
           and thread it through and report it?
        -}
        exposedItemToDeclaration : Module Typed.LocatedExpr -> ExposedItem -> Maybe (Declaration Typed.LocatedExpr)
        exposedItemToDeclaration module_ item =
            Dict.get (Exposing.name item) module_.declarations

        globalPath : List (Declaration Typed.LocatedExpr)
        globalPath =
            findPath
                graph
                exposedDeclarations
    in
    globalPath
        |> Dict.groupBy .module_


{-| Generic function to find a good ordering of values (so that all the
dependencies are emitted before the
-}
findPath : Graph -> Set ( ModuleName, VarName ) -> List (Declaration Typed.LocatedExpr)
findPath graph startingDeclarations =
    let
        edgesToFollow =
            Graph.alongIncomingEdges

        alwaysEmittedDeclarations =
            []

        appendNodeLabel ctx list =
            ctx.node.label :: list

        startingPoints =
            findDeclarations
                graph
                startingDeclarations
    in
    Graph.guidedDfs
        edgesToFollow
        (Graph.onDiscovery appendNodeLabel)
        startingPoints
        alwaysEmittedDeclarations
        graph
        |> {- ignore the untraversed path (dead code elimination!) -} Tuple.first


findDeclarations : Graph -> Set ( ModuleName, VarName ) -> List Int
findDeclarations graph declarations =
    Graph.nodes graph
        |> List.filterMap
            (\{ id, label } ->
                if Set.member ( label.module_, label.name ) declarations then
                    Just id

                else
                    Nothing
            )


type alias Dependency =
    { from : Declaration Typed.LocatedExpr
    , to : Declaration Typed.LocatedExpr
    }


modulesToGraph : ModuleName -> Modules Typed.LocatedExpr -> Result EmitError Graph
modulesToGraph mainModuleName modules =
    {- TODO this is probably a bit far off, but... how to allow for cyclic
       dependencies in lambdas but not in exposed expressions?
    -}
    let
        maybeMainDeclaration : Maybe (Declaration Typed.LocatedExpr)
        maybeMainDeclaration =
            Dict.get mainModuleName modules
                |> Maybe.andThen (.declarations >> Dict.get (VarName.fromString "main"))
    in
    maybeMainDeclaration
        |> Result.fromMaybe MainDeclarationNotFound
        |> Result.andThen
            (\mainDeclaration ->
                collectDependencies
                    modules
                    [ mainDeclaration ]
                    Set.empty
                    []
            )
        |> Result.map
            (\( declarations, dependencies ) ->
                let
                    declarationList : List (Declaration Typed.LocatedExpr)
                    declarationList =
                        Set.toList declarations

                    declarationIndexes : Dict (Declaration Typed.LocatedExpr) Int
                    declarationIndexes =
                        declarationList
                            |> List.indexedMap (\i declaration -> ( declaration, i ))
                            -- It's important that the order of declarationList doesn't change after we do this!
                            -- Graph.fromNodeLabelsAndEdgePairs depends on the indexes!
                            |> Dict.fromList

                    dependenciesList : List ( Int, Int )
                    dependenciesList =
                        dependencies
                            |> List.filterMap
                                (\{ from, to } ->
                                    -- {from,to} --> `from` depends on `to`
                                    -- the Graph library needs the other direction though,
                                    -- so we switch them here:
                                    Maybe.map2 Tuple.pair
                                        (Dict.get to declarationIndexes)
                                        (Dict.get from declarationIndexes)
                                )
                in
                Graph.fromNodeLabelsAndEdgePairs declarationList dependenciesList
            )


collectDependencies :
    Modules Typed.LocatedExpr
    -> List (Declaration Typed.LocatedExpr)
    -> Set (Declaration Typed.LocatedExpr)
    -> List Dependency
    -> Result EmitError ( Set (Declaration Typed.LocatedExpr), List Dependency )
collectDependencies modules remainingDeclarations doneDeclarations doneDependencies =
    -- TODO arguments in a record for better clarity... the usages of this function look weird
    {- TODO maybe keep a dict around so that we don't do the same work twice if
       two declarations depend on the same declaration
    -}
    case remainingDeclarations of
        [] ->
            Ok ( doneDeclarations, doneDependencies )

        currentDeclaration :: restOfDeclarations ->
            if Set.member currentDeclaration doneDeclarations then
                -- nothing new
                collectDependencies
                    modules
                    restOfDeclarations
                    doneDeclarations
                    doneDependencies

            else
                findDependencies modules currentDeclaration.body
                    |> Result.andThen
                        (\newDeclarations ->
                            let
                                newDependencies =
                                    newDeclarations
                                        |> List.map
                                            (\dependency ->
                                                { from = currentDeclaration
                                                , to = dependency
                                                }
                                            )
                            in
                            -- remember the found dependencies, but also remember to collect *their* dependencies!
                            collectDependencies
                                modules
                                (restOfDeclarations ++ newDeclarations)
                                (Set.insert currentDeclaration doneDeclarations)
                                (doneDependencies ++ newDependencies)
                        )


findDependencies : Modules Typed.LocatedExpr -> DeclarationBody Typed.LocatedExpr -> Result EmitError (List (Declaration Typed.LocatedExpr))
findDependencies modules declarationBody =
    case declarationBody of
        Value locatedExpr ->
            findDependenciesOfExpr modules locatedExpr

        TypeAlias { definition } ->
            findDependenciesOfType modules definition

        CustomType { constructors } ->
            constructors
                |> List.concatMap (.arguments >> List.map (findDependenciesOfTypeArgument modules))
                |> Result.combine
                |> Result.map List.concat


findDependenciesOfTypeArgument : Modules Typed.LocatedExpr -> TypeArgument -> Result EmitError (List (Declaration Typed.LocatedExpr))
findDependenciesOfTypeArgument modules typeArgument =
    case typeArgument of
        ConcreteType type_ ->
            findDependenciesOfType modules type_

        TypeParameter _ ->
            Ok []


findDependenciesOfType : Modules Typed.LocatedExpr -> Type -> Result EmitError (List (Declaration Typed.LocatedExpr))
findDependenciesOfType modules type_ =
    let
        findDependencies_ =
            findDependenciesOfType modules
    in
    case type_ of
        Type.Var id ->
            Ok []

        Type.Function t1 t2 ->
            Result.map2 (++)
                (findDependencies_ t1)
                (findDependencies_ t2)

        Type.Int ->
            Ok []

        Type.Float ->
            Ok []

        Type.Char ->
            Ok []

        Type.String ->
            Ok []

        Type.Bool ->
            Ok []

        Type.List t1 ->
            findDependencies_ t1

        Type.Unit ->
            Ok []

        Type.Tuple t1 t2 ->
            Result.map2 (++)
                (findDependencies_ t1)
                (findDependencies_ t2)

        Type.Tuple3 t1 t2 t3 ->
            Result.map3 (\d1 d2 d3 -> d1 ++ d2 ++ d3)
                (findDependencies_ t1)
                (findDependencies_ t2)
                (findDependencies_ t3)

        Type.UserDefinedType ( moduleName, varName ) paramTypes ->
            let
                typeDependencies =
                    modules
                        |> Dict.get moduleName
                        |> Result.fromMaybe (ModuleNotFound moduleName varName)
                        |> Result.andThen
                            (.declarations
                                >> Dict.get varName
                                >> Result.fromMaybe (DeclarationNotFound moduleName varName)
                            )
                        |> Result.andThen (.body >> findDependencies modules)

                paramsDependencies =
                    paramTypes
                        |> List.map findDependencies_
                        |> Result.combine
                        |> Result.map List.concat
            in
            Result.map2 (++)
                typeDependencies
                paramsDependencies


findDependenciesOfExpr : Modules Typed.LocatedExpr -> Typed.LocatedExpr -> Result EmitError (List (Declaration Typed.LocatedExpr))
findDependenciesOfExpr modules locatedExpr =
    let
        findDependencies_ =
            findDependenciesOfExpr modules
    in
    case Typed.getExpr locatedExpr of
        Literal _ ->
            Ok []

        Var { qualifier, name } ->
            modules
                |> Dict.get qualifier
                |> Result.fromMaybe (ModuleNotFound qualifier name)
                |> Result.andThen (.declarations >> Dict.get name >> Result.fromMaybe (DeclarationNotFound qualifier name))
                |> Result.map List.singleton

        Argument _ ->
            Ok []

        Plus e1 e2 ->
            Result.map2 (++)
                (findDependencies_ e1)
                (findDependencies_ e2)

        Cons e1 e2 ->
            Result.map2 (++)
                (findDependencies_ e1)
                (findDependencies_ e2)

        Lambda { argument, body } ->
            findDependencies_ body
                |> Result.map (List.filter (\decl -> decl.name /= argument))

        Call { fn, argument } ->
            Result.map2 (++)
                (findDependencies_ fn)
                (findDependencies_ argument)

        If { test, then_, else_ } ->
            Result.map3 (\d1 d2 d3 -> d1 ++ d2 ++ d3)
                (findDependencies_ test)
                (findDependencies_ then_)
                (findDependencies_ else_)

        Let { bindings, body } ->
            let
                bindingsDependencies =
                    bindings
                        |> Dict.values
                        |> List.map (.body >> findDependencies_)
                        |> Result.combine
                        |> Result.map List.concat
            in
            Result.map2 (++)
                bindingsDependencies
                (findDependencies_ body)

        List items ->
            List.map findDependencies_ items
                |> Result.combine
                |> Result.map List.concat

        Unit ->
            Ok []

        Tuple e1 e2 ->
            Result.map2 (++)
                (findDependencies_ e1)
                (findDependencies_ e2)

        Tuple3 e1 e2 e3 ->
            Result.map3 (\d1 d2 d3 -> d1 ++ d2 ++ d3)
                (findDependencies_ e1)
                (findDependencies_ e2)
                (findDependencies_ e3)
