module Stage.Optimize.Boilerplate exposing (asModulesIn, optimizeModule)

import AST.Canonical as Canonical
import AST.Typed as Typed
import Common
import Common.Types
    exposing
        ( Dict_
        , Module
        , Modules
        , Project
        , TopLevelDeclaration
        , VarName
        )
import Dict.Any exposing (AnyDict)
import Error exposing (Error(..), TypeError)
import Extra.Dict.Any


asModulesIn : Project Typed.ProjectFields -> Modules Typed.Expr -> Project Typed.ProjectFields
asModulesIn project modules =
    { project | modules = modules }


optimizeModule : (Typed.Expr -> Typed.Expr) -> Module Typed.Expr -> Module Typed.Expr
optimizeModule optimizeExpr module_ =
    module_.topLevelDeclarations
        |> Dict.Any.map (always (optimizeTopLevelDeclaration optimizeExpr))
        |> asTopLevelDeclarationsIn module_


asTopLevelDeclarationsIn : Module Typed.Expr -> Dict_ VarName (TopLevelDeclaration Typed.Expr) -> Module Typed.Expr
asTopLevelDeclarationsIn module_ topLevelDeclarations =
    { module_ | topLevelDeclarations = topLevelDeclarations }


optimizeTopLevelDeclaration : (Typed.Expr -> Typed.Expr) -> TopLevelDeclaration Typed.Expr -> TopLevelDeclaration Typed.Expr
optimizeTopLevelDeclaration optimizeExpr decl =
    optimizeExpr decl.body
        |> asBodyIn decl


asBodyIn : TopLevelDeclaration Typed.Expr -> Typed.Expr -> TopLevelDeclaration Typed.Expr
asBodyIn decl body =
    { decl | body = body }
