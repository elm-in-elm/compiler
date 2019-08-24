module Stage.Parse exposing
    ( checkModuleNameAndFilePath
    , parse
    )

import AST.Frontend as Frontend
import Data.FileContents as FileContents exposing (FileContents)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Module exposing (Module)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Error exposing (Error(..), GeneralError(..), ParseError(..))
import Parser.Advanced as P
import Stage.Parse.Parser as Parser


{-| This `parse` function is used only by cli/, not by library/.
Maybe we should use it in library/ too?
-}
parse : FilePath -> FileContents -> Result Error (Module Frontend.LocatedExpr)
parse filePath fileContents =
    P.run
        (Parser.module_ filePath)
        (FileContents.toString fileContents)
        |> Result.mapError (ParseError << ParseProblem)


{-| TODO maybe there should be a "Checks" module for checks across phases?
-}
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
