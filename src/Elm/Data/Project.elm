module Elm.Data.Project exposing (Project)

{-| The project information (corresponds to a single elm.json file).
Holds more information though - the data in the type parameter holds all the
parsed modules etc. - what every stage needs. See `ProjectFields` of the various
AST stages.

@docs Project

-}

import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.ModuleName exposing (ModuleName)
import {- elm/project-metadata-utils -} Elm.Project


{-| Each AST stage has its own project fields - that's what the `r` parameter is.
Eg. on the frontend we have `program : Modules Frontend.Expr`
and on the backend we have `graph : Backend.Graph`.
-}
type alias Project projectFields =
    {- TODO holds the source directory twice - once in sourceDirectory and once
       in the parsed elm.json info in elmJson?
    -}
    -- TODO mainFilePath and mainModuleName should live in CLI instead of here?
    { projectFields
        | elmJson : Elm.Project.Project
        , mainFilePath : FilePath
        , mainModuleName : ModuleName
        , sourceDirectory : FilePath
    }
