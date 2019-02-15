module Stage.PrepareForBackend exposing (prepareForBackend)

import AST.Backend as Backend
import AST.Canonical as Canonical
import Common.Types exposing (Modules, Project)
import Error exposing (Error)
import Graph


prepareForBackend : Project Canonical.ProjectFields -> Result Error (Project Backend.ProjectFields)
prepareForBackend p =
    Ok
        { mainFilePath = p.mainFilePath
        , mainModuleName = p.mainModuleName
        , elmJson = p.elmJson
        , sourceDirectory = p.sourceDirectory
        , programGraph = modulesToGraph p.program
        }


modulesToGraph : Modules Canonical.Expr -> Backend.Graph
modulesToGraph modules =
    -- TODO
    Graph.empty
