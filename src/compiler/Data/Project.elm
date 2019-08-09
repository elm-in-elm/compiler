module Data.Project exposing
    ( Project
    , ProjectToEmit
    )

import Data.FileContents exposing (FileContents)
import Data.FilePath exposing (FilePath)
import Data.ModuleName exposing (ModuleName)
import {- elm/project-metadata-utils -} Elm.Project


{-| Each AST stage has its own project fields - that's what the `r` parameter is.
Eg. on the frontend we have `program : Modules Frontend.Expr`
and on the backend we have `graph : Backend.Graph`.
-}
type alias Project r =
    { r
        | elmJson : Elm.Project.Project
        , mainFilePath : FilePath
        , mainModuleName : ModuleName
        , sourceDirectory : FilePath
    }


type alias ProjectToEmit =
    { output : FileContents }
