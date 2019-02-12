module AST.Common exposing
    ( Literal(..)
    , TopLevelDeclaration
    , VarName(..)
    , varNameToString
    )


type VarName
    = VarName String


varNameToString : VarName -> String
varNameToString (VarName varName) =
    varName


type Literal
    = LInt Int


type alias TopLevelDeclaration expr =
    { name : VarName
    , body : expr
    }
