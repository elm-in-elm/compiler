module Elm.Compiler.Stage.Parse exposing (parse)

import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (Error(..), ParseError(..))
import Elm.Compiler.Stage.Parse.Parser as Parser
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName as ModuleName exposing (ModuleName)
import Parser.Advanced as P


{-| This `parse` function is used only by cli/, not by library/.
Maybe we should use it in library/ too?
-}
parse : { filePath : FilePath, fileContents : FileContents } -> Result Error (Module Frontend.LocatedExpr)
parse { filePath, fileContents } =
    P.run
        (Parser.module_ filePath)
        fileContents
        |> Result.mapError (ParseError << ParseProblem)
