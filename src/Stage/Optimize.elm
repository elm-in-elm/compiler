module Stage.Optimize exposing (optimize)

import AST.Common.Literal as Literal
import AST.Common.Type as Type
import AST.Typed as Typed
import Common.Types exposing (Project)
import Error exposing (Error)
import Stage.Optimize.Boilerplate as Boilerplate


optimize : Project Typed.ProjectFields -> Result x (Project Typed.ProjectFields)
optimize project =
    Ok (Boilerplate.optimizeProject optimizeExpr project)


optimizeExpr : Typed.Expr -> Typed.Expr
optimizeExpr expr =
    Typed.transformAll
        [ optimizePlus
        , optimizeIfLiteralBool
        ]
        expr


optimizePlus : Typed.Expr -> Maybe Typed.Expr
optimizePlus ( expr, _ ) =
    case expr of
        Typed.Plus ( Typed.Literal (Literal.Int left), _ ) ( Typed.Literal (Literal.Int right), _ ) ->
            Just
                ( Typed.Literal (Literal.Int (left + right))
                , Type.Int
                )

        _ ->
            Nothing


optimizeIfLiteralBool : Typed.Expr -> Maybe Typed.Expr
optimizeIfLiteralBool ( expr, _ ) =
    case expr of
        Typed.If { test, then_, else_ } ->
            case test of
                ( Typed.Literal (Literal.Bool bool), _ ) ->
                    Just
                        (if bool then
                            then_

                         else
                            else_
                        )

                _ ->
                    Nothing

        _ ->
            Nothing
