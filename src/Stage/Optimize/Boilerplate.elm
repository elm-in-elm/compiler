module Stage.Optimize.Boilerplate exposing
    ( optimizeModule
    , optimizeProject
    )

import Dict exposing (Dict)
import Elm.AST.Typed as Typed
import Elm.Data.Declaration as Declaration exposing (Declaration, DeclarationBody)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.VarName exposing (VarName)


optimizeProject :
    (Typed.LocatedExpr -> Typed.LocatedExpr)
    -> Project Typed.ProjectFields
    -> Project Typed.ProjectFields
optimizeProject optimizeExpr project =
    project.modules
        |> Dict.map (always (optimizeModule optimizeExpr))
        |> asModulesIn project


asModulesIn :
    Project Typed.ProjectFields
    -> Dict ModuleName (Module Typed.LocatedExpr Never Qualified)
    -> Project Typed.ProjectFields
asModulesIn project modules =
    { project | modules = modules }


optimizeModule :
    (Typed.LocatedExpr -> Typed.LocatedExpr)
    -> Module Typed.LocatedExpr Never Qualified
    -> Module Typed.LocatedExpr Never Qualified
optimizeModule optimizeExpr module_ =
    module_.declarations
        |> Dict.map (always (optimizeDeclaration optimizeExpr))
        |> asDeclarationsIn module_


asDeclarationsIn :
    Module Typed.LocatedExpr Never Qualified
    -> Dict VarName (Declaration Typed.LocatedExpr Never Qualified)
    -> Module Typed.LocatedExpr Never Qualified
asDeclarationsIn module_ declarations =
    { module_ | declarations = declarations }


optimizeDeclaration :
    (Typed.LocatedExpr -> Typed.LocatedExpr)
    -> Declaration Typed.LocatedExpr Never Qualified
    -> Declaration Typed.LocatedExpr Never Qualified
optimizeDeclaration optimizeExpr decl =
    decl.body
        |> Declaration.mapBody
            optimizeExpr
            identity
            identity
        |> asBodyIn decl


asBodyIn :
    Declaration Typed.LocatedExpr Never Qualified
    -> DeclarationBody Typed.LocatedExpr Never Qualified
    -> Declaration Typed.LocatedExpr Never Qualified
asBodyIn decl body =
    { decl | body = body }
