module AST.Common exposing
    ( Literal(..)
    , TopLevelDeclaration
    , VarName(..)
    )


type VarName
    = VarName String


type Literal
    = LInt Int
      -- TODO | LFloat Float
    | LBool Bool -- TODO how to do this and have Bools defined in the elm/core instead of hardcoded in the compiler?


type alias TopLevelDeclaration expr =
    { name : VarName
    , body : expr
    }
