module Stage.Emit exposing (emit)

import AST.Backend as Backend
import AST.Common
    exposing
        ( TopLevelDeclaration
        , VarName(..)
        )
import Common.Types
    exposing
        ( FileContents(..)
        , Project
        , ProjectToEmit
        )
import Dict.Any
import Error exposing (EmitError(..), Error(..))


emit : Project Backend.Expr -> Result Error ProjectToEmit
emit project =
    project.program
        |> Dict.Any.get project.mainModuleName
        |> Result.fromMaybe (EmitError MainModuleNotFound)
        |> Result.andThen
            (.topLevelDeclarations
                >> Dict.Any.get (VarName "main")
                >> Result.fromMaybe (EmitError MainDeclarationNotFound)
            )
        |> Result.map (emitTopLevelDeclaration >> FileContents >> ProjectToEmit)


emitTopLevelDeclaration : TopLevelDeclaration Backend.Expr -> String
emitTopLevelDeclaration declaration =
    "// TODO"



--emitExpr : Backend.Expr -> String
