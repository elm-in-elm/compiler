module Stage.Parse exposing
    ( checkModuleNameAndFilePath
    , parse
    )

import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (Error(..), GeneralError(..), ParseError(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName as ModuleName exposing (ModuleName)
import Parser.Advanced as P
import Stage.Parse.Parser as Parser


{-| This `parse` function is used only by cli/, not by library/.
Maybe we should use it in library/ too?
-}
parse : { filePath : FilePath, fileContents : FileContents } -> Result Error (Module Frontend.LocatedExpr)
parse { filePath, fileContents } =
    P.run
        (Parser.module_ filePath)
        fileContents
        |> Result.mapError (ParseError << ParseProblem)


{-| TODO maybe there should be a "Checks" module for checks across phases?
-}
checkModuleNameAndFilePath : { sourceDirectory : FilePath, filePath : FilePath } -> Module Frontend.LocatedExpr -> Result Error (Module Frontend.LocatedExpr)
checkModuleNameAndFilePath { sourceDirectory, filePath } ({ name } as parsedModule) =
    let
        expectedName : Result Error ModuleName
        expectedName =
            ModuleName.expectedModuleName
                { sourceDirectory = sourceDirectory
                , filePath = filePath
                }
                |> Result.fromMaybe (GeneralError (FileNotInSourceDirectories filePath))
    in
    if expectedName == Ok name then
        Ok parsedModule

    else
        Err
            (ParseError
                (ModuleNameDoesntMatchFilePath
                    { moduleName = name
                    , filePath = filePath
                    }
                )
            )
