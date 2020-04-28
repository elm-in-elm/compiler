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
    -> Dict ModuleName (Module Typed.LocatedExpr Never ModuleName)
    -> Project Typed.ProjectFields
asModulesIn project modules =
    { project | modules = modules }


optimizeModule :
    (Typed.LocatedExpr -> Typed.LocatedExpr)
    -> Module Typed.LocatedExpr Never ModuleName
    -> Module Typed.LocatedExpr Never ModuleName
optimizeModule optimizeExpr module_ =
    module_.declarations
        |> Dict.map (always (optimizeDeclaration optimizeExpr))
        |> asDeclarationsIn module_


asDeclarationsIn :
    Module Typed.LocatedExpr Never ModuleName
    -> Dict VarName (Declaration Typed.LocatedExpr Never ModuleName)
    -> Module Typed.LocatedExpr Never ModuleName
asDeclarationsIn module_ declarations =
    { module_ | declarations = declarations }


optimizeDeclaration :
    (Typed.LocatedExpr -> Typed.LocatedExpr)
    -> Declaration Typed.LocatedExpr Never ModuleName
    -> Declaration Typed.LocatedExpr Never ModuleName
optimizeDeclaration optimizeExpr decl =
    decl.body
        |> Declaration.mapBody optimizeExpr
        |> asBodyIn decl


asBodyIn :
    Declaration Typed.LocatedExpr Never ModuleName
    -> DeclarationBody Typed.LocatedExpr ModuleName
    -> Declaration Typed.LocatedExpr Never ModuleName
asBodyIn decl body =
    { decl | body = body }
