module Stage.Optimize.Boilerplate exposing
    ( optimizeModule
    , optimizeProject
    )

import Dict exposing (Dict)
import Elm.AST.Typed as Typed
import Elm.Data.Declaration as Declaration exposing (Declaration, DeclarationBody(..))
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
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
    -> Dict ModuleName (Module Typed.LocatedExpr)
    -> Project Typed.ProjectFields
asModulesIn project modules =
    { project | modules = modules }


optimizeModule :
    (Typed.LocatedExpr -> Typed.LocatedExpr)
    -> Module Typed.LocatedExpr
    -> Module Typed.LocatedExpr
optimizeModule optimizeExpr module_ =
    module_.declarations
        |> Dict.map (always (optimizeDeclaration optimizeExpr))
        |> asDeclarationsIn module_


asDeclarationsIn :
    Module Typed.LocatedExpr
    -> Dict VarName (Declaration Typed.LocatedExpr)
    -> Module Typed.LocatedExpr
asDeclarationsIn module_ declarations =
    { module_ | declarations = declarations }


optimizeDeclaration :
    (Typed.LocatedExpr -> Typed.LocatedExpr)
    -> Declaration Typed.LocatedExpr
    -> Declaration Typed.LocatedExpr
optimizeDeclaration optimizeExpr decl =
    decl.body
        |> Declaration.mapBody optimizeExpr
        |> asBodyIn decl


asBodyIn :
    Declaration Typed.LocatedExpr
    -> DeclarationBody Typed.LocatedExpr
    -> Declaration Typed.LocatedExpr
asBodyIn decl body =
    { decl | body = body }
