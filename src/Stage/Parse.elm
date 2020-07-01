module Stage.Parse exposing (parse)

import Elm.AST.Frontend as Frontend
import Elm.Compiler.Error exposing (Error(..), ParseError(..))
import Elm.Data.FileContents exposing (FileContents)
import Elm.Data.FilePath exposing (FilePath)
import Elm.Data.Module exposing (Module)
import Elm.Data.Qualifiedness exposing (PossiblyQualified)
import Elm.Data.TypeAnnotation exposing (TypeAnnotation)
import Parser.Advanced as P
import Stage.Parse.Parser as Parser


{-| This `parse` function is used only by cli/, not by src/ (library).
Would it make sense to use it in src/ too?
-}
parse :
    { filePath : FilePath, fileContents : FileContents }
    -> Result Error (Module Frontend.LocatedExpr TypeAnnotation PossiblyQualified)
parse { filePath, fileContents } =
    P.run (Parser.module_ filePath) fileContents
        |> Result.mapError
            (\errorList -> ParseError (ParseProblem ( errorList, fileContents )))
