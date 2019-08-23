module Stage.Optimize.Boilerplate exposing (optimizeProject)

import AST.Typed as Typed
import AssocList as Dict exposing (Dict)
import Data.Declaration as Declaration exposing (Declaration, DeclarationBody(..))
import Data.Module exposing (Module, Modules)
import Data.Project exposing (Project)
import Data.VarName exposing (VarName)


optimizeProject : (Typed.LocatedExpr -> Typed.LocatedExpr) -> Project Typed.ProjectFields -> Project Typed.ProjectFields
optimizeProject optimizeExpr project =
    project.modules
        |> Dict.map (always (optimizeModule optimizeExpr))
        |> asModulesIn project


asModulesIn : Project Typed.ProjectFields -> Modules Typed.LocatedExpr -> Project Typed.ProjectFields
asModulesIn project modules =
    { project | modules = modules }


optimizeModule : (Typed.LocatedExpr -> Typed.LocatedExpr) -> Module Typed.LocatedExpr -> Module Typed.LocatedExpr
optimizeModule optimizeExpr module_ =
    module_.declarations
        |> Dict.map (always (optimizeDeclaration optimizeExpr))
        |> asDeclarationsIn module_


asDeclarationsIn : Module Typed.LocatedExpr -> Dict VarName (Declaration Typed.LocatedExpr) -> Module Typed.LocatedExpr
asDeclarationsIn module_ declarations =
    { module_ | declarations = declarations }


optimizeDeclaration : (Typed.LocatedExpr -> Typed.LocatedExpr) -> Declaration Typed.LocatedExpr -> Declaration Typed.LocatedExpr
optimizeDeclaration optimizeExpr decl =
    decl.body
        |> Declaration.mapBody optimizeExpr
        |> asBodyIn decl


asBodyIn : Declaration Typed.LocatedExpr -> DeclarationBody Typed.LocatedExpr -> Declaration Typed.LocatedExpr
asBodyIn decl body =
    { decl | body = body }
