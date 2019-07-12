module Stage.Optimize.Boilerplate exposing (optimizeProject)

import AST.Typed as Typed
import Common.Types
    exposing
        ( Dict_
        , Module
        , Modules
        , Project
        , TopLevelDeclaration
        , VarName
        )
import Dict.Any


optimizeProject : (Typed.LocatedExpr -> Typed.LocatedExpr) -> Project Typed.ProjectFields -> Project Typed.ProjectFields
optimizeProject optimizeExpr project =
    project.modules
        |> Dict.Any.map (always (optimizeModule optimizeExpr))
        |> asModulesIn project


asModulesIn : Project Typed.ProjectFields -> Modules Typed.LocatedExpr -> Project Typed.ProjectFields
asModulesIn project modules =
    { project | modules = modules }


optimizeModule : (Typed.LocatedExpr -> Typed.LocatedExpr) -> Module Typed.LocatedExpr -> Module Typed.LocatedExpr
optimizeModule optimizeExpr module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (always (optimizeTopLevelDeclaration optimizeExpr))
        |> asTopLevelDeclarationsIn module_


asTopLevelDeclarationsIn : Module Typed.LocatedExpr -> Dict_ VarName (TopLevelDeclaration Typed.LocatedExpr) -> Module Typed.LocatedExpr
asTopLevelDeclarationsIn module_ topLevelDeclarations =
    { module_ | topLevelDeclarations = topLevelDeclarations }


optimizeTopLevelDeclaration : (Typed.LocatedExpr -> Typed.LocatedExpr) -> TopLevelDeclaration Typed.LocatedExpr -> TopLevelDeclaration Typed.LocatedExpr
optimizeTopLevelDeclaration optimizeExpr decl =
    optimizeExpr decl.body
        |> asBodyIn decl


asBodyIn : TopLevelDeclaration Typed.LocatedExpr -> Typed.LocatedExpr -> TopLevelDeclaration Typed.LocatedExpr
asBodyIn decl body =
    { decl | body = body }
