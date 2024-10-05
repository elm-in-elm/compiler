module Stage.Optimize exposing
    ( defaultOptimizations
    , optimize
    , optimizeExpr
    , optimizeExprWith
    )

import Elm.AST.Typed as Typed
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Project exposing (Project)
import Elm.Data.VarName exposing (VarName)
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
        Typed.BinOp "+" l r ->
            case ( Typed.getExpr l, Typed.getExpr r ) of
                ( Typed.Int left, Typed.Int right ) ->
                    Just (Typed.setExpr (Typed.Int (left + right)) r)

                ( Typed.Float left, Typed.Float right ) ->
                    Just (Typed.setExpr (Typed.Float (left + right)) r)

                _ ->
                    Nothing

        _ ->
            Nothing


optimizeCons : Typed.LocatedExpr -> Maybe Typed.LocatedExpr
optimizeCons located =
    case Typed.getExpr located of
        Typed.Call c1 ->
            case ( Typed.getExpr c1.fn, Typed.getExpr c1.argument ) of
                ( Typed.Call c2, Typed.List list ) ->
                    case Typed.getExpr c2.fn of
                        Typed.Var v1 ->
                            if v1 == { module_ = "List", name = "cons" } then
                                Just (Typed.setExpr (Typed.List (c2.argument :: list)) located)

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


optimizeIfLiteralBool : Typed.LocatedExpr -> Maybe Typed.LocatedExpr
optimizeIfLiteralBool located =
    case Typed.getExpr located of
        Typed.If { test, then_, else_ } ->
            case Typed.getExpr test of
                Typed.ConstructorValue c ->
                    getBoolValue c
                        |> Maybe.map
                            (\bool ->
                                if bool then
                                    then_

                                else
                                    else_
                            )

                _ ->
                    Nothing

        _ ->
            Nothing


getBoolValue : { module_ : ModuleName, name : VarName } -> Maybe Bool
getBoolValue { module_, name } =
    if module_ == "Basics" && name == "True" then
        Just True

    else if module_ == "Basics" && name == "False" then
        Just False

    else
        Nothing
