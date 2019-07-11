module AST.Common.Type exposing
    ( Type(..)
    , getVarId
    , toString
    )

{-| In its own module because both Error.TypeError and AST.Typed need to see it
-}


type Type
    = Var Int
    | Function Type Type
    | Int
    | Float
    | Char
    | String
    | Bool
    | List Type
    | Unit


getVarId : Type -> Maybe Int
getVarId type_ =
    case type_ of
        Var id ->
            Just id

        _ ->
            Nothing


toString : Type -> String
toString type_ =
    case type_ of
        Var int ->
            "t" ++ String.fromInt int

        Function t1 t2 ->
            toString t1 ++ " -> " ++ toString t2

        Int ->
            "Int"

        Float ->
            "Float"

        Char ->
            "Char"

        String ->
            "String"

        Bool ->
            "Bool"

        List t1 ->
            "List " ++ toString t1

        Unit ->
            "()"
