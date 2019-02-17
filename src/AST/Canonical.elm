module AST.Canonical exposing
    ( Expr(..)
    , ProjectFields
    )

import AST.Common exposing (Literal)
import Common.Types
    exposing
        ( ModuleName
        , Modules
        , VarName
        )


type alias ProjectFields =
    { modules : Modules Expr }


{-| Differs from Frontend.Expr by:

  - having fully qualified variables

-}
type Expr
    = Literal Literal
    | Var ( ModuleName, VarName )
    | Plus Expr Expr
