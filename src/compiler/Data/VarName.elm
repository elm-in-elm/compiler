module Data.VarName exposing
    ( VarName
    , fromString
    , toString
    )


type VarName
    = VarName String


toString : VarName -> String
toString (VarName varName) =
    varName


fromString : String -> VarName
fromString string =
    VarName string
