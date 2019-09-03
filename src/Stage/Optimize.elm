module Stage.Optimize exposing
    ( defaultOptimizations
    , optimize
    , optimizeExpr
    , optimizeExprWith
    )

import AST.Common.Literal as Literal
import AST.Typed as Typed
import Data.Project exposing (Project)
import Stage.Optimize.Boilerplate as Boilerplate


optimize : Project Typed.ProjectFields -> Project Typed.ProjectFields
optimize project =
    Boilerplate.optimizeProject optimizeExpr project


optimizeExpr : Typed.LocatedExpr -> Typed.LocatedExpr
optimizeExpr locatedExpr =
    optimizeExprWith defaultOptimizations locatedExpr


optimizeExprWith : List (Typed.LocatedExpr -> Maybe Typed.LocatedExpr) -> Typed.LocatedExpr -> Typed.LocatedExpr
optimizeExprWith optimizations locatedExpr =
    Typed.transformAll optimizations locatedExpr


defaultOptimizations : List (Typed.LocatedExpr -> Maybe Typed.LocatedExpr)
defaultOptimizations =
    -- TODO these will have to use `mapUnwrapped` and have different types
    [ optimizePlus
    , optimizeCons
    , optimizeIfLiteralBool
    ]


{-| TODO try to create some helpers for the optimization passes to make them
not care about the location that much...
-}
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
