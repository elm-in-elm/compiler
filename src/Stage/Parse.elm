module Stage.Parse exposing (parse)

import AST.Common exposing (TopLevelDeclaration, VarName)
import AST.Frontend as Frontend
import Common
    exposing
        ( Dict_
        , FileContents(..)
        , FilePath(..)
        , Module
        , ModuleName
        , Project
        , Set_
        )
import Error exposing (Error, ParseError(..))


{-| I suspect in the future we'll have to add an argument of previously parsed
modules.
-}
parse : Project -> FilePath -> FileContents -> Result Error (Module Frontend.Expr)
parse { sourceDirectory } filePath fileContents =
    let
        expectedModuleName : Maybe ModuleName
        expectedModuleName =
            -- TODO is it the same as the one in the actual file contents?
            Common.expectedModuleName sourceDirectory filePath

        dependencies : Set_ ModuleName
        dependencies =
            Debug.todo "parse - dependencies"

        name : ModuleName
        name =
            Debug.todo "parse - name"

        topLevelDeclarations : Dict_ VarName (TopLevelDeclaration Frontend.Expr)
        topLevelDeclarations =
            Debug.todo "parse - topLevelDeclarations"
    in
    Ok
        { dependencies = dependencies
        , name = name
        , filePath = filePath
        , topLevelDeclarations = topLevelDeclarations
        }
