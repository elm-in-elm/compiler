module Stage.PrepareForBackend exposing (prepareForBackend)

import AST.Backend as Backend
import AST.Canonical as Canonical
import AST.Frontend as Frontend exposing (Expr(..), Literal(..))
import Common
import Common.Types
    exposing
        ( ModuleName
        , Modules
        , Project
        , TopLevelDeclaration
        , VarName(..)
        )
import Dict.Any exposing (AnyDict)
import Error
    exposing
        ( Error(..)
        , PrepareForBackendError(..)
        )
import Graph
import Set.Any exposing (AnySet)


prepareForBackend : Project Canonical.ProjectFields -> Result Error (Project Backend.ProjectFields)
prepareForBackend p =
    modulesToGraph p.mainModuleName p.program
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
    { from : TopLevelDeclaration Canonical.Expr
    , to : TopLevelDeclaration Canonical.Expr
    }


modulesToGraph : ModuleName -> Modules Canonical.Expr -> Result PrepareForBackendError Backend.Graph
modulesToGraph mainModuleName modules =
    -- TODO this is probably a bit far off, but... how to allow for cyclic
    -- dependencies in lambdas but not in exposed expressions?
    let
        maybeMainDeclaration : Maybe (TopLevelDeclaration Canonical.Expr)
        maybeMainDeclaration =
            Dict.Any.get mainModuleName modules
                |> Maybe.andThen (.topLevelDeclarations >> Dict.Any.get (VarName "main"))
    in
    maybeMainDeclaration
        |> Result.fromMaybe MainDeclarationNotFound
        |> Result.map
            (\mainDeclaration ->
                let
                    ( declarations, dependencies ) =
                        collectTopLevelDependencies
                            [ mainDeclaration ]
                            (Set.Any.empty Common.topLevelDeclarationToString)
                            []

                    declarationList : List (TopLevelDeclaration Canonical.Expr)
                    declarationList =
                        declarations
                            |> Set.Any.toList

                    declarationIndexes : AnyDict String (TopLevelDeclaration Canonical.Expr) Int
                    declarationIndexes =
                        declarationList
                            |> List.indexedMap (\i declaration -> ( declaration, i ))
                            -- It's important that the order of declarationList doesn't change after we do this!
                            -- Graph.fromNodeLabelsAndEdgePairs depends on the indexes!
                            |> Dict.Any.fromList Common.topLevelDeclarationToString

                    dependenciesList : List ( Int, Int )
                    dependenciesList =
                        dependencies
                            |> List.filterMap
                                (\{ from, to } ->
                                    Maybe.map2 Tuple.pair
                                        (Dict.Any.get from declarationIndexes)
                                        (Dict.Any.get to declarationIndexes)
                                )
                in
                Graph.fromNodeLabelsAndEdgePairs declarationList dependenciesList
            )


collectTopLevelDependencies :
    List (TopLevelDeclaration Canonical.Expr)
    -> AnySet String (TopLevelDeclaration Canonical.Expr)
    -> List Dependency
    -> ( AnySet String (TopLevelDeclaration Canonical.Expr), List Dependency )
collectTopLevelDependencies remainingDeclarations doneDeclarations doneDependencies =
    -- TODO maybe keep a dict around so that we don't do the same work twice if
    -- two declarations depend on the same declaration
    case remainingDeclarations of
        [] ->
            ( doneDeclarations, doneDependencies )

        currentDeclaration :: restOfDeclarations ->
            if Set.Any.member currentDeclaration doneDeclarations then
                -- nothing new
                collectTopLevelDependencies
                    restOfDeclarations
                    doneDeclarations
                    doneDependencies

            else
                let
                    newDeclarations =
                        findDependencies currentDeclaration.body

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
                collectTopLevelDependencies
                    (restOfDeclarations ++ newDeclarations)
                    (Set.Any.insert currentDeclaration doneDeclarations)
                    (doneDependencies ++ newDependencies)


findDependencies : Canonical.Expr -> List (TopLevelDeclaration Canonical.Expr)
findDependencies expr =
    case expr of
        Literal (LInt _) ->
            []
