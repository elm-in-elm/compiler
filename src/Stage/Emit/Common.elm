module Stage.Emit.Common exposing
    ( mangleModuleName
    , mangleQualifiedVar
    , mangleVarName
    , prepareProjectFields
    )

import Elm.AST.Typed as Typed
import Elm.Compiler.Error exposing (Error(..))
import Elm.Data.Declaration exposing (Declaration)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.VarName exposing (VarName)
import Stage.Emit as Emit


type alias ProjectFields =
    { declarationList : List (Declaration Typed.LocatedExpr Never Qualified) }


prepareProjectFields : Project Typed.ProjectFields -> Result Error (Project ProjectFields)
prepareProjectFields project =
    Emit.projectToDeclarationList project
        |> Result.mapError EmitError
        |> Result.map
            (\declarationList ->
                { mainFilePath = project.mainFilePath
                , mainModuleName = project.mainModuleName
                , elmJson = project.elmJson
                , sourceDirectories = project.sourceDirectories
                , declarationList = declarationList
                }
            )


mangleQualifiedVar : { module_ : ModuleName, name : VarName } -> String
mangleQualifiedVar { module_, name } =
    mangleModuleName module_ ++ "$" ++ mangleVarName name


mangleModuleName : ModuleName -> String
mangleModuleName moduleName =
    String.replace "." "$" moduleName


mangleVarName : VarName -> String
mangleVarName varName =
    -- TODO this does nothing currently... what does the official Elm compiler do?
    varName
