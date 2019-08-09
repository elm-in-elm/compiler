module Stage.Parse exposing (parse)

import AST.Frontend as Frontend
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Module exposing (Module)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.Project exposing (Project)
import Error exposing (Error(..), GeneralError(..), ParseError(..))
import Parser.Advanced as P
import Stage.Parse.Parser as Parser


parse : Project a -> FilePath -> FileContents -> Result Error (Module Frontend.LocatedExpr)
parse { sourceDirectory } filePath fileContents =
    P.run (Parser.module_ filePath) (FileContents.toString fileContents)
        |> Result.mapError (ParseError << ParseProblem)
        |> Result.andThen
            (checkModuleNameAndFilePath
                { sourceDirectory = sourceDirectory
                , filePath = filePath
                }
            )


checkModuleNameAndFilePath : { sourceDirectory : FilePath, filePath : FilePath } -> Module Frontend.LocatedExpr -> Result Error (Module Frontend.LocatedExpr)
checkModuleNameAndFilePath { sourceDirectory, filePath } ({ name } as parsedModule) =
    let
        expectedName : Result Error ModuleName
        expectedName =
            ModuleName.expected
                { sourceDirectory = FilePath.toString sourceDirectory
                , filePath = FilePath.toString filePath
                }
                |> Result.fromMaybe (GeneralError (FileNotInSourceDirectories filePath))
    in
    if expectedName == Ok name then
        Ok parsedModule

    else
        Err (ParseError (ModuleNameDoesntMatchFilePath name filePath))
