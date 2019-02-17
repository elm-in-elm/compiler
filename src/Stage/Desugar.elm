module Stage.Desugar exposing (desugar)

import AST.Canonical as Canonical
import AST.Frontend as Frontend
import Common
import Common.Types
    exposing
        ( Module
        , ModuleName
        , Project
        , TopLevelDeclaration
        , VarName
        )
import Dict.Any exposing (AnyDict)
import Error exposing (DesugarError(..), Error(..))


desugar : Project Frontend.ProjectFields -> Result Error (Project Canonical.ProjectFields)
desugar p =
    p.modules
        |> resultMapDict Common.moduleNameToString desugarModule
        |> Result.map
            (\modules ->
                { elmJson = p.elmJson
                , mainFilePath = p.mainFilePath
                , mainModuleName = p.mainModuleName
                , sourceDirectory = p.sourceDirectory
                , modules = modules
                }
            )
        |> Result.mapError DesugarError


desugarModule : Module Frontend.Expr -> Result DesugarError (Module Canonical.Expr)
desugarModule m =
    m.topLevelDeclarations
        |> resultMapDict Common.varNameToString (desugarTopLevelDeclaration m)
        |> Result.map
            (\topLevelDeclarations ->
                { dependencies = m.dependencies
                , name = m.name
                , filePath = m.filePath
                , type_ = m.type_
                , exposing_ = m.exposing_
                , topLevelDeclarations = topLevelDeclarations
                }
            )


{-| Roughly: Dict.Any.map toResult >> Result.Extra.combine
We might need to make the function accept keys if there arises a need for it.
-}
resultMapDict : (k -> comparable) -> (v -> Result x v2) -> AnyDict comparable k v -> Result x (AnyDict comparable k v2)
resultMapDict toComparable fn dict =
    dict
        |> Dict.Any.toList
        -- This following line is a mouthful. It uses the Result-producing fn on the second part of the tuple,
        -- and in the same loop does what Result.Extra.combine would did with that second part of the tuple:
        -- List (a, Result x b) -> Result x (List (a,b))
        |> List.foldr (\( a, b ) acc -> Result.map2 (\b_ acc_ -> ( a, b_ ) :: acc_) (fn b) acc) (Ok [])
        |> Result.map (Dict.Any.fromList toComparable)


desugarTopLevelDeclaration : Module Frontend.Expr -> TopLevelDeclaration Frontend.Expr -> Result DesugarError (TopLevelDeclaration Canonical.Expr)
desugarTopLevelDeclaration module_ d =
    desugarExpr module_ d.body
        |> Result.map
            (\body ->
                { name = d.name
                , module_ = d.module_
                , body = body
                }
            )


{-|

    - Var VarName -> Var (ModuleName, VarName)

-}
desugarExpr : Module Frontend.Expr -> Frontend.Expr -> Result DesugarError Canonical.Expr
desugarExpr module_ expr =
    case expr of
        Frontend.Literal literal ->
            Ok (Canonical.Literal literal)

        Frontend.Var varName ->
            findModuleOfVar module_ varName
                |> Result.fromMaybe (VarNotInEnvOfModule varName module_.name)
                |> Result.map (\moduleName -> Canonical.Var ( moduleName, varName ))

        Frontend.Plus e1 e2 ->
            Result.map2 Canonical.Plus
                (desugarExpr module_ e1)
                (desugarExpr module_ e2)


{-| TODO Currently we don't look for vars in other imported modules. Do it!
-}
findModuleOfVar : Module Frontend.Expr -> VarName -> Maybe ModuleName
findModuleOfVar module_ varName =
    if Dict.Any.member varName module_.topLevelDeclarations then
        Just module_.name

    else
        Nothing
