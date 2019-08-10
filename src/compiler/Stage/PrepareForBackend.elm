module Stage.PrepareForBackend exposing (prepareForBackend)

import AST.Backend as Backend
import AST.Common.Literal exposing (Literal(..))
import AST.Typed as Typed exposing (Expr_(..))
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Data.Declaration exposing (Declaration)
import Data.Module exposing (Modules)
import Data.ModuleName exposing (ModuleName)
import Data.Project exposing (Project)
import Data.VarName as VarName
import Error
    exposing
        ( Error(..)
        , PrepareForBackendError(..)
        )
import Graph


prepareForBackend : Project Typed.ProjectFields -> Result Error (Project Backend.ProjectFields)
prepareForBackend p =
    modulesToGraph p.mainModuleName p.modules
        |> Result.mapError PrepareForBackendError
        |> Result.map
            (\programGraph ->
                { mainFilePath = p.mainFilePath
                , mainModuleName = p.mainModuleName
                , elmJson = p.elmJson
                , sourceDirectory = p.sourceDirectory
                , programGraph = programGraph
                }
            )


type alias Dependency =
    { from : Declaration Typed.LocatedExpr
    , to : Declaration Typed.LocatedExpr
    }


modulesToGraph : ModuleName -> Modules Typed.LocatedExpr -> Result PrepareForBackendError Backend.Graph
modulesToGraph mainModuleName modules =
    -- TODO this is probably a bit far off, but... how to allow for cyclic
    -- dependencies in lambdas but not in exposed expressions?
    let
        maybeMainDeclaration : Maybe (Declaration Typed.LocatedExpr)
        maybeMainDeclaration =
            Dict.get mainModuleName modules
                |> Maybe.andThen (.declarations >> Dict.get (VarName.fromString "main"))
    in
    maybeMainDeclaration
        |> Result.fromMaybe MainDeclarationNotFound
        |> Result.map
            (\mainDeclaration ->
                let
                    ( declarations, dependencies ) =
                        collectDependencies
                            modules
                            [ mainDeclaration ]
                            Set.empty
                            []

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
    -> ( Set (Declaration Typed.LocatedExpr), List Dependency )
collectDependencies modules remainingDeclarations doneDeclarations doneDependencies =
    -- TODO maybe keep a dict around so that we don't do the same work twice if
    -- two declarations depend on the same declaration
    case remainingDeclarations of
        [] ->
            ( doneDeclarations, doneDependencies )

        currentDeclaration :: restOfDeclarations ->
            if Set.member currentDeclaration doneDeclarations then
                -- nothing new
                collectDependencies
                    modules
                    restOfDeclarations
                    doneDeclarations
                    doneDependencies

            else
                let
                    newDeclarations =
                        findDependencies modules currentDeclaration.body

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


findDependencies : Modules Typed.LocatedExpr -> Typed.LocatedExpr -> List (Declaration Typed.LocatedExpr)
findDependencies modules located =
    let
        findDependencies_ =
            findDependencies modules
    in
    case Typed.getExpr located of
        Literal _ ->
            []

        Var { qualifier, name } ->
            modules
                |> Dict.get qualifier
                |> Maybe.andThen (.declarations >> Dict.get name)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        Argument _ ->
            []

        Plus e1 e2 ->
            findDependencies_ e1
                ++ findDependencies_ e2

        Cons e1 e2 ->
            findDependencies_ e1
                ++ findDependencies_ e2

        Lambda { argument, body } ->
            findDependencies_ body
                |> List.filter (\decl -> decl.name /= argument)

        Call { fn, argument } ->
            findDependencies_ fn
                ++ findDependencies_ argument

        If { test, then_, else_ } ->
            findDependencies_ test
                ++ findDependencies_ then_
                ++ findDependencies_ else_

        Let { bindings, body } ->
            let
                bindingsDependencies =
                    bindings
                        |> Dict.values
                        |> List.concatMap (.body >> findDependencies_)
            in
            bindingsDependencies
                ++ findDependencies_ body

        List items ->
            List.concatMap findDependencies_ items

        Unit ->
            []

        Tuple e1 e2 ->
            findDependencies_ e1 ++ findDependencies_ e2

        Tuple3 e1 e2 e3 ->
            findDependencies_ e1 ++ findDependencies_ e2 ++ findDependencies_ e3
