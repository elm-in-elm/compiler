module Stage.RemoveTypes exposing (removeTypes)

import AST.Canonical as Canonical
import AST.Typed as Typed
import Common.Types exposing (Project)
import Error exposing (Error)
import Stage.RemoveTypes.Boilerplate as Boilerplate


removeTypes : Project Typed.ProjectFields -> Result Error (Project Canonical.ProjectFields)
removeTypes project =
    Ok (Boilerplate.removeTypesInProject removeTypesInExpr project)


{-| TODO recursion schemes! Potentially a variant of transformOnce that works on (a -> b) instead of (a -> a) ?
-}
removeTypesInExpr : Typed.Expr -> Canonical.Expr
removeTypesInExpr ( expr, _ ) =
    case expr of
        Typed.Literal lit ->
            Canonical.Literal lit

        Typed.Var var ->
            Canonical.Var var

        Typed.Argument name ->
            Canonical.Argument name

        Typed.Plus e1 e2 ->
            Canonical.Plus
                (removeTypesInExpr e1)
                (removeTypesInExpr e2)

        Typed.Lambda { argument, body } ->
            Canonical.lambda
                argument
                (removeTypesInExpr body)

        Typed.Call { fn, argument } ->
            Canonical.Call
                { fn = removeTypesInExpr fn
                , argument = removeTypesInExpr argument
                }

        Typed.If { test, then_, else_ } ->
            Canonical.If
                { test = removeTypesInExpr test
                , then_ = removeTypesInExpr then_
                , else_ = removeTypesInExpr else_
                }
