module Todo exposing (..)

--------------
--- USAGE: ---
--------------

exprString : String
exprString =
    """
if isOK model then
    "YAY!"
else
    "Boo..."
    """


astForElmFormat : Result Error Frontend.Expr
astForElmFormat =
    exprString
        |> Elm.Compiler.parseExpr 
        |> Random.andThen (Elm.Compiler.desugarExpr NoPrelude)
        |> Random.andThen Elm.Compiler.

----------------------------------------
--- GIVES US CONSTRAINTS ON THE API: ---
----------------------------------------

desugarExpr : ImplicitImports -> Frontend.Expr -> Result Error Canonical.Expr

type ImplicitImports
    = NoPrelude
    | Prelude
    | PreludeAndCustom (Dict ... ...?)
