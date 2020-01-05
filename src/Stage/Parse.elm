module Stage.Parse exposing (parse)

import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (Error(..), ParseError(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module exposing (Module)
import Elm.Data.ModuleName as ModuleName exposing (ModuleName)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Parser.Advanced as P
import Stage.Parse.Parser as Parser


{-| This `parse` function is used only by cli/, not by library/.
Maybe we should use it in library/ too?
-}
parse : { filePath : FilePath, fileContents : FileContents } -> Result Error (Module Frontend.LocatedExpr TypeAnnotation)
parse { filePath, fileContents } =
    Result.mapError
        (\errorList -> ParseError (ParseProblem ( errorList, fileContents )))
        (P.run (Parser.module_ filePath) fileContents)
