module AST.Typechecked exposing
    ( Expr
    , Expr_(..)
    , ProjectFields
    )

import AST.Common exposing (Literal)
import AST.Type exposing (Type)
import Common.Types
    exposing
        ( ModuleName
        , Modules
        , VarName
        )


type alias ProjectFields =
    { modules : Modules Expr }


{-| Differs from Canonical.Expr by:

  - being a tuple of the underlying Expr\_ type and its type

-}
type alias Expr =
    ( Expr_, Type )


type Expr_
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Lambda { argument : VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
