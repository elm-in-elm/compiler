module Stage.Parse exposing (parse)

import Common
    exposing
        ( Dict_
        , FileContents(..)
        , FilePath(..)
        , Module
        , ModuleName
        , Project
        , Set_
        , TopLevelDeclaration
        , VarName
        )
import Error exposing (Error, ParseError(..))



-- TODO check filename too


{-| I suspect in the future we'll have to add an argument of previously parsed
modules.
-}
parse : Project -> FilePath -> FileContents -> Result Error Module
parse { sourceDirectory } filePath fileContents =
    let
        expectedModuleName : Maybe ModuleName
        expectedModuleName =
            Common.expectedModuleName sourceDirectory filePath

        dependencies : Set_ ModuleName
        dependencies =
            Debug.todo "parse - dependencies"

        name : ModuleName
        name =
            Debug.todo "parse - name"

        topLevelDeclarations : Dict_ VarName TopLevelDeclaration
        topLevelDeclarations =
            Debug.todo "parse - topLevelDeclarations"
    in
    Ok
        { dependencies = dependencies
        , name = name
        , filePath = filePath
        , topLevelDeclarations = topLevelDeclarations
        }
