module Stage.Optimize exposing
    ( defaultOptimizations
    , optimize
    , optimizeExpr
    , optimizeExprWith
    )

import Elm.AST.Common.Literal as Literal
import Elm.AST.Typed as Typed
import Elm.Data.Project exposing (Project)
import Stage.Optimize.Boilerplate as Boilerplate


optimize : Project Typed.ProjectFields -> Project Typed.ProjectFields
optimize project =
    Boilerplate.optimizeProject optimizeExpr project


optimizeExpr : Typed.LocatedExpr -> Typed.LocatedExpr
optimizeExpr locatedExpr =
    optimizeExprWith defaultOptimizations locatedExpr


optimizeExprWith : List ( String, Typed.LocatedExpr -> Maybe Typed.LocatedExpr ) -> Typed.LocatedExpr -> Typed.LocatedExpr
optimizeExprWith optimizations locatedExpr =
    Typed.transformAll
        (List.map Tuple.second optimizations)
        locatedExpr


defaultOptimizations : List ( String, Typed.LocatedExpr -> Maybe Typed.LocatedExpr )
defaultOptimizations =
    [ ( "plus", optimizePlus )
    , ( "cons", optimizeCons )
    , ( "if-literal-bool", optimizeIfLiteralBool )
    ]


optimizePlus : Typed.LocatedExpr -> Maybe Typed.LocatedExpr
optimizePlus located =
    case Typed.getExpr located of
        Typed.Plus l r ->
            case ( Typed.getExpr l, Typed.getExpr r ) of
                ( Typed.Literal (Literal.Int left), Typed.Literal (Literal.Int right) ) ->
                    Just (Typed.mapExpr (\_ -> Typed.Literal (Literal.Int (left + right))) r)

                _ ->
                    Nothing

        _ ->
            Nothing


optimizeCons : Typed.LocatedExpr -> Maybe Typed.LocatedExpr
optimizeCons located =
    case Typed.getExpr located of
        Typed.Cons l r ->
            case Typed.getExpr r of
                Typed.List list ->
                    Just (Typed.mapExpr (\_ -> Typed.List (l :: list)) r)

                _ ->
                    Nothing

        _ ->
            Nothing


optimizeIfLiteralBool : Typed.LocatedExpr -> Maybe Typed.LocatedExpr
optimizeIfLiteralBool located =
    case Typed.getExpr located of
        Typed.If { test, then_, else_ } ->
            case Typed.getExpr test of
                Typed.Literal (Literal.Bool bool) ->
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
