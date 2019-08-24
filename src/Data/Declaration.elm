module Data.Declaration exposing
    ( Constructor
    , Declaration
    , DeclarationBody(..)
    , combine
    , map
    , mapBody
    , toString
    )

import AST.Common.Type exposing (Type, TypeArgument)
import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)


type alias Declaration expr =
    { module_ : ModuleName
    , name : VarName
    , body : DeclarationBody expr
    }


type DeclarationBody expr
    = Value expr
    | TypeAlias
        { parameters : List String -- on the left side of =
        , definition : Type
        }
    | CustomType
        { parameters : List String -- on the left side of =
        , constructors : List Constructor
        }


type alias Constructor =
    { name : String
    , arguments : List TypeArgument
    }


toString : Declaration a -> String
toString { name, module_ } =
    ModuleName.toString module_ ++ "." ++ VarName.toString name


map : (a -> b) -> Declaration a -> Declaration b
map fn declaration =
    { module_ = declaration.module_
    , name = declaration.name
    , body = mapBody fn declaration.body
    }


mapBody : (a -> b) -> DeclarationBody a -> DeclarationBody b
mapBody fn body =
    case body of
        Value expr ->
            Value <| fn expr

        TypeAlias r ->
            TypeAlias r

        CustomType r ->
            CustomType r


combine : DeclarationBody (Result err a) -> Result err (DeclarationBody a)
combine body =
    case body of
        Value result ->
            Result.map Value result

        TypeAlias r ->
            Ok <| TypeAlias r

        CustomType r ->
            Ok <| CustomType r
