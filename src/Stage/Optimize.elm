module Stage.Optimize exposing (optimize)

import AST.Common.Literal as Literal
import AST.Common.Type as Type
import AST.Typed as Typed
import Common.Types exposing (Project)
import Dict.Any
import Error exposing (Error)
import Stage.Optimize.Boilerplate exposing (asModulesIn, optimizeModule)


optimize : Project Typed.ProjectFields -> Result Error (Project Typed.ProjectFields)
optimize project =
    -- TODO do we need types when optimizing? If not we could swap the stages and make this a bit simpler
    project.modules
        |> Dict.Any.map (always (optimizeModule optimizeExpr))
        |> asModulesIn project
        |> Ok


optimizeExpr : Typed.Expr -> Typed.Expr
optimizeExpr expr =
    Typed.transformAll
        [ optimizePlus
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
