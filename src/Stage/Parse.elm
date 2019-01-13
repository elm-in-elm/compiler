module Stage.Parse exposing (parse)

import AST.Common exposing (TopLevelDeclaration, VarName)
import AST.Frontend as Frontend
import Common
    exposing
        ( Dict_
        , FileContents(..)
        , Module
        , Project
        , Set_
        )
import Common.Types
    exposing
        ( FilePath(..)
        , ModuleName
        )
import Error exposing (Error(..), ParseError(..))


{-| I suspect in the future we'll have to add an argument of previously parsed
modules.
-}
parse : Project -> FilePath -> FileContents -> Result Error (Module Frontend.Expr)
parse { sourceDirectory } filePath fileContents =
    let
        expectedModuleName : Result Error ModuleName
        expectedModuleName =
            -- TODO is it the same as the one in the actual file contents?
            Common.expectedModuleName sourceDirectory filePath

        dependencies : Set_ ModuleName
        dependencies =
            Debug.todo "parse - dependencies"

        actualModuleName : ModuleName
        actualModuleName =
            Debug.todo "parse - actualModuleName"

        topLevelDeclarations : Dict_ VarName (TopLevelDeclaration Frontend.Expr)
        topLevelDeclarations =
            Debug.todo "parse - topLevelDeclarations"
    in
    if expectedModuleName == Ok actualModuleName then
        Ok
            { dependencies = dependencies
            , name = actualModuleName
            , filePath = filePath
            , topLevelDeclarations = topLevelDeclarations
            }

    else
        ModuleNameDoesntMatchFileName actualModuleName filePath
            |> ParseError
            |> Err
