module Stage.Emit exposing
    ( projectToDeclarationList
    , modulesToDeclarationLists
    )

{-| Helpers for the various Stage.Emit.<LANGUAGE> modules.

If you're looking for emit functions for a specific target language,
look into Stage.Emit.<LANGUAGE> modules.

There are two emit usecases we know of:

    1. all modules to one output file, dead code eliminated (eg. Elm to JS)

@docs projectToDeclarationList

    2. each module to its own output file, dead code eliminated (eg. Elm to Elixir?)
    The fact that we don't have `main` function(s) doesn't stop us - we can think
    of all the _exposed functions_ from all modules as potential entrypoints.

@docs modulesToDeclarationLists

There _might_ be usecases for not eliminating dead code? We don't do that
currently because we don't know of any, but if there are, please raise an issue
in the GitHub repo!

TODO we'll probably have to detect cycles and do something like IIFE

-}

import AssocList
import AssocSet
import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Compiler.Error exposing (EmitError(..))
import Elm.Data.Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.Exposing as Exposing exposing (ExposedItem, Exposing(..))
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified(..))
import Elm.Data.Type.Concrete as ConcreteType exposing (ConcreteType)
import Elm.Data.VarName exposing (VarName)
import Graph
import List.NonEmpty
import OurExtras.List as List
import Result.Extra as Result
import Set exposing (Set)


projectToDeclarationList :
    Project Typed.ProjectFields
    -> Result EmitError (List (Declaration Typed.LocatedExpr Never Qualified))
projectToDeclarationList { mainModuleName, modules } =
    modulesToGraph mainModuleName modules
        |> Result.map (findPathToMain mainModuleName)


modulesToDeclarationLists :
    Project Typed.ProjectFields
    -> Result EmitError (Dict ModuleName (List (Declaration Typed.LocatedExpr Never Qualified)))
modulesToDeclarationLists ({ mainModuleName, modules } as project) =
    modulesToGraph mainModuleName modules
        |> Result.map (findPathForEachModule project)


type alias Graph =
    Graph.Graph (Declaration Typed.LocatedExpr Never Qualified) ()


{-| We want to be able to emit `main`. We only emit what's needed for that.
Taken from the example in elm-community/graph README :sweat\_smile:
-}
findPathToMain : ModuleName -> Graph -> List (Declaration Typed.LocatedExpr Never Qualified)
findPathToMain mainModuleName programGraph =
    findPath
        programGraph
        (Set.singleton ( mainModuleName, "main" ))


{-| In this case we don't have `main`s but TODO finish writing this
-}
findPathForEachModule :
    Project Typed.ProjectFields
    -> Graph
    -> Dict ModuleName (List (Declaration Typed.LocatedExpr Never Qualified))
findPathForEachModule project graph =
    let
        exposedDeclarations : Set ( ModuleName, VarName )
        exposedDeclarations =
            project.modules
                |> Dict.values
                |> List.fastConcatMap moduleToExposedDeclarations
                |> List.map (\decl -> ( decl.module_, decl.name ))
                |> Set.fromList

        moduleToExposedDeclarations :
            Module Typed.LocatedExpr Never Qualified
            -> List (Declaration Typed.LocatedExpr Never Qualified)
        moduleToExposedDeclarations module_ =
            case module_.exposing_ of
                ExposingAll ->
                    Dict.values module_.declarations

                ExposingSome exposedItems ->
                    exposedItems
                        |> List.NonEmpty.toList
                        -- throwing away stuff - see comment for `exposedItemToDeclaration`
                        |> List.filterMap (exposedItemToDeclaration module_)

        {- We'll be throwing away Nothings created here - those happen if the
           compiler can't find a definition that is supposed to be exposed.

           Maybe we should return a Result here (Err (DeclarationExposedButNotDefined ...))
           and thread it through and report it?
        -}
        exposedItemToDeclaration :
            Module Typed.LocatedExpr Never Qualified
            -> ExposedItem
            -> Maybe (Declaration Typed.LocatedExpr Never Qualified)
        exposedItemToDeclaration module_ item =
            Dict.get (Exposing.name item) module_.declarations

        globalPath : List (Declaration Typed.LocatedExpr Never Qualified)
        globalPath =
            findPath
                graph
                exposedDeclarations
    in
    globalPath
        |> Dict.groupBy .module_


{-| Generic function to find a good ordering of values (so that all the
dependencies are emitted before the TODO finish writing this
-}
findPath : Graph -> Set ( ModuleName, VarName ) -> List (Declaration Typed.LocatedExpr Never Qualified)
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
    { from : Declaration Typed.LocatedExpr Never Qualified
    , to : Declaration Typed.LocatedExpr Never Qualified
    }


modulesToGraph :
    ModuleName
    -> Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Result EmitError Graph
modulesToGraph mainModuleName modules =
    {- TODO this is probably a bit far off, but... how to allow for cyclic
       dependencies in lambdas but not in exposed expressions?
    -}
    let
        maybeMainDeclaration : Maybe (Declaration Typed.LocatedExpr Never Qualified)
        maybeMainDeclaration =
            Dict.get mainModuleName modules
                |> Maybe.andThen (.declarations >> Dict.get "main")
    in
    maybeMainDeclaration
        |> Result.fromMaybe MainDeclarationNotFound
        |> Result.andThen
            (\mainDeclaration ->
                collectDependencies
                    modules
                    [ mainDeclaration ]
                    AssocSet.empty
                    []
            )
        |> Result.map
            (\( declarations, dependencies ) ->
                let
                    declarationList : List (Declaration Typed.LocatedExpr Never Qualified)
                    declarationList =
                        AssocSet.toList declarations

                    declarationIndexes : AssocList.Dict (Declaration Typed.LocatedExpr Never Qualified) Int
                    declarationIndexes =
                        declarationList
                            |> List.indexedMap (\i declaration -> ( declaration, i ))
                            {- It's important that the order of declarationList
                               doesn't change after we do this!
                               Graph.fromNodeLabelsAndEdgePairs depends on the indexes!
                            -}
                            |> AssocList.fromList

                    dependenciesList : List ( Int, Int )
                    dependenciesList =
                        dependencies
                            |> List.filterMap
                                (\{ from, to } ->
                                    {- {from,to} --> `from` depends on `to`.
                                       The Graph library needs the other
                                       direction though, so we switch them here:
                                    -}
                                    Maybe.map2 Tuple.pair
                                        (AssocList.get to declarationIndexes)
                                        (AssocList.get from declarationIndexes)
                                )
                in
                Graph.fromNodeLabelsAndEdgePairs declarationList dependenciesList
            )


collectDependencies :
    Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> List (Declaration Typed.LocatedExpr Never Qualified)
    -> AssocSet.Set (Declaration Typed.LocatedExpr Never Qualified)
    -> List Dependency
    -> Result EmitError ( AssocSet.Set (Declaration Typed.LocatedExpr Never Qualified), List Dependency )
collectDependencies modules remainingDeclarations doneDeclarations doneDependencies =
    -- TODO arguments in a record for better clarity... the usages of this function look weird
    {- TODO maybe keep a dict around so that we don't do the same work twice if
       two declarations depend on the same declaration
    -}
    case remainingDeclarations of
        [] ->
            Ok ( doneDeclarations, doneDependencies )

        currentDeclaration :: restOfDeclarations ->
            if AssocSet.member currentDeclaration doneDeclarations then
                -- nothing new
                collectDependencies
                    modules
                    restOfDeclarations
                    doneDeclarations
                    doneDependencies

            else
                Dict.get currentDeclaration.module_ modules
                    |> Result.fromMaybe
                        (ModuleNotFoundForVar
                            { module_ = currentDeclaration.module_
                            , name = currentDeclaration.name
                            }
                        )
                    |> Result.andThen
                        (\currentModule ->
                            findDependencies
                                modules
                                currentModule
                                currentDeclaration.body
                        )
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
                            {- remember the found dependencies, but also remember
                               to collect *their* dependencies!
                            -}
                            collectDependencies
                                modules
                                (restOfDeclarations ++ newDeclarations)
                                (AssocSet.insert currentDeclaration doneDeclarations)
                                (doneDependencies ++ newDependencies)
                        )


findDependencies :
    Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Module Typed.LocatedExpr Never Qualified
    -> DeclarationBody Typed.LocatedExpr Never Qualified
    -> Result EmitError (List (Declaration Typed.LocatedExpr Never Qualified))
findDependencies modules thisModule declarationBody =
    case declarationBody of
        Value { expression } ->
            findDependenciesOfExpr modules expression

        TypeAlias { definition } ->
            {- we don't have to think about parameters; those are always
               variables and not concrete types here
            -}
            findDependenciesOfType modules thisModule definition

        CustomType { constructors } ->
            {- we don't have to think about parameters; those are always
               variables and not concrete types here
            -}
            constructors
                |> List.NonEmpty.toList
                |> List.fastConcatMap
                    (.arguments
                        >> List.map
                            (findDependenciesOfType
                                modules
                                thisModule
                            )
                    )
                |> Result.combine
                |> Result.map List.concat

        Port _ ->
            Ok []


findDependenciesOfType :
    Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Module Typed.LocatedExpr Never Qualified
    -> ConcreteType Qualified
    -> Result EmitError (List (Declaration Typed.LocatedExpr Never Qualified))
findDependenciesOfType modules thisModule type_ =
    let
        f =
            findDependenciesOfType modules thisModule
    in
    case type_ of
        ConcreteType.TypeVar _ ->
            Ok []

        ConcreteType.Function { from, to } ->
            Result.map2 (++)
                (f from)
                (f to)

        ConcreteType.Int ->
            Ok []

        ConcreteType.Float ->
            Ok []

        ConcreteType.Char ->
            Ok []

        ConcreteType.String ->
            Ok []

        ConcreteType.Bool ->
            Ok []

        ConcreteType.List t1 ->
            f t1

        ConcreteType.Unit ->
            Ok []

        ConcreteType.Tuple t1 t2 ->
            Result.map2 (++)
                (f t1)
                (f t2)

        ConcreteType.Tuple3 t1 t2 t3 ->
            Result.map3 (\d1 d2 d3 -> d1 ++ d2 ++ d3)
                (f t1)
                (f t2)
                (f t3)

        ConcreteType.UserDefinedType { qualifiedness, name, args } ->
            let
                argsDependencies =
                    args
                        |> List.map f
                        |> Result.combine
                        |> Result.map List.concat

                (Qualified moduleName) =
                    qualifiedness

                typeDependencies =
                    findDependenciesOfVar
                        modules
                        thisModule
                        moduleName
                        name
            in
            Result.map2 (++)
                typeDependencies
                argsDependencies

        ConcreteType.Record bindings ->
            bindings
                |> Dict.values
                |> List.map f
                |> Result.combine
                |> Result.map List.concat


findDependenciesOfVar :
    Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Module Typed.LocatedExpr Never Qualified
    -> ModuleName
    -> VarName
    -> Result EmitError (List (Declaration Typed.LocatedExpr Never Qualified))
findDependenciesOfVar modules thisModule moduleName varName =
    Dict.get moduleName modules
        |> Result.fromMaybe
            (ModuleNotFoundForVar
                { module_ = moduleName
                , name = varName
                }
            )
        |> Result.andThen
            (\module_ ->
                Dict.get varName module_.declarations
                    |> Result.fromMaybe
                        (DeclarationNotFound
                            { module_ = moduleName
                            , name = varName
                            }
                        )
            )
        |> Result.andThen (.body >> findDependencies modules thisModule)


findDependenciesOfExpr :
    Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Typed.LocatedExpr
    -> Result EmitError (List (Declaration Typed.LocatedExpr Never Qualified))
findDependenciesOfExpr modules locatedExpr =
    let
        f =
            findDependenciesOfExpr modules
    in
    case Typed.getExpr locatedExpr of
        Int _ ->
            Ok []

        Float _ ->
            Ok []

        Char _ ->
            Ok []

        String _ ->
            Ok []

        Bool _ ->
            Ok []

        Var ({ module_, name } as var) ->
            modules
                |> Dict.get module_
                |> Result.fromMaybe (ModuleNotFoundForVar var)
                |> Result.andThen
                    (.declarations
                        >> Dict.get name
                        >> Result.fromMaybe (DeclarationNotFound var)
                    )
                |> Result.map List.singleton

        Argument _ ->
            Ok []

        BinOp _ e1 e2 ->
            Result.map2 (++)
                (f e1)
                (f e2)

        Lambda { argument, body } ->
            f body
                |> Result.map (List.filter (\decl -> decl.name /= argument))

        Call { fn, argument } ->
            Result.map2 (++)
                (f fn)
                (f argument)

        If { test, then_, else_ } ->
            Result.map3 (\d1 d2 d3 -> d1 ++ d2 ++ d3)
                (f test)
                (f then_)
                (f else_)

        Let { bindings, body } ->
            let
                bindingsDependencies =
                    bindings
                        |> Dict.values
                        |> List.map (.body >> f)
                        |> Result.combine
                        |> Result.map List.concat
            in
            Result.map2 (++)
                bindingsDependencies
                (f body)

        List items ->
            List.map f items
                |> Result.combine
                |> Result.map List.concat

        Unit ->
            Ok []

        Tuple e1 e2 ->
            Result.map2 (++)
                (f e1)
                (f e2)

        Tuple3 e1 e2 e3 ->
            Result.map3 (\d1 d2 d3 -> d1 ++ d2 ++ d3)
                (f e1)
                (f e2)
                (f e3)

        Record bindings ->
            bindings
                |> Dict.values
                |> List.map (.body >> f)
                |> Result.combine
                |> Result.map List.concat

        RecordAccess e field ->
            f e

        Case e branches ->
            let
                branchesDependencies =
                    branches
                        |> List.NonEmpty.toList
                        |> List.map (.body >> f)
                        |> Result.combine
                        |> Result.map List.concat
            in
            Result.map2 (++)
                (f e)
                branchesDependencies

        ConstructorValue ({ module_, name } as rec) ->
            modules
                |> Dict.get module_
                |> Result.fromMaybe (ModuleNotFoundForVar rec)
                |> Result.andThen
                    (.declarations
                        >> Dict.get name
                        >> Result.fromMaybe (DeclarationNotFound rec)
                    )
                |> Result.map List.singleton
