module Stage.Emit exposing (emit)

import AST.Backend as Backend
import AST.Common
    exposing
        ( TopLevelDeclaration
        , VarName(..)
        )
import AST.Frontend
    exposing
        ( Expr(..)
        , Literal(..)
        )
import Common.Types
    exposing
        ( FileContents(..)
        , Project
        , ProjectToEmit
        )
import Dict.Any
import Error exposing (EmitError(..), Error(..))


{-| TODO emit everything from every module, not just Main's main
TODO check for dependencies and emit in dependency order
TODO dead code elimination
-}
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
emitTopLevelDeclaration { name, body } =
    let
        (VarName varName) =
            name
    in
    "const " ++ varName ++ " = " ++ emitExpr body ++ ";"


emitExpr : Backend.Expr -> String
emitExpr expr =
    case expr of
        Literal (LInt int) ->
            String.fromInt int
