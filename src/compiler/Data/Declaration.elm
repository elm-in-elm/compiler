module Data.Declaration exposing
    ( Declaration
    , toString
    )

import Data.ModuleName as ModuleName exposing (ModuleName)
import Data.VarName as VarName exposing (VarName)


type alias Declaration expr =
    { module_ : ModuleName
    , name : VarName
    , body : expr
    }


toString : Declaration a -> String
toString { name, module_ } =
    ModuleName.toString module_ ++ "." ++ VarName.toString name
