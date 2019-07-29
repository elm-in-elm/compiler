module Stage.Optimize exposing (optimize, optimizeExpr)

import AST.Common.Literal as Literal
import AST.Common.Located as Located
import AST.Common.Type as Type
import AST.Typed as Typed
import Common.Types exposing (Project)
import Stage.Optimize.Boilerplate as Boilerplate


optimize : Project Typed.ProjectFields -> Project Typed.ProjectFields
optimize project =
    Boilerplate.optimizeProject optimizeExpr project


optimizeExpr : Typed.LocatedExpr -> Typed.LocatedExpr
optimizeExpr located =
    Typed.transformAll
        [ optimizePlus
        , optimizeCons
        , optimizeIfLiteralBool
        ]
        located


{-| TODO try to create some helpers for the optimization passes to make them
not care about the location that much...
-}
optimizePlus : Typed.LocatedExpr -> Maybe Typed.LocatedExpr
optimizePlus located =
    case Typed.getExpr located of
        Typed.Plus l r ->
            case ( Typed.getExpr l, Typed.getExpr r ) of
                ( Typed.Literal (Literal.Int left), Typed.Literal (Literal.Int right) ) ->
                    Just
                        (Located.replaceWith
                            ( Typed.Literal (Literal.Int (left + right))
                            , Type.Int
                            )
                            located
                        )

                _ ->
                    Nothing

        _ ->
            Nothing


log : String -> a -> b -> b
log label data returned =
    Debug.log label data |> (\_ -> returned)


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
