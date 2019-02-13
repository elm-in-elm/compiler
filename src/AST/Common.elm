module AST.Common exposing
    ( TopLevelDeclaration
    , VarName(..)
    , varNameToString
    )


type VarName
    = VarName String


varNameToString : VarName -> String
varNameToString (VarName varName) =
    varName


type alias TopLevelDeclaration expr =
    { name : VarName
    , body : expr
    }
