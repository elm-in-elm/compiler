module AST.Common.Type exposing
    ( Type(..)
    , getVarId
    , isNotParametric
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


isNotParametric : Type -> Bool
isNotParametric type_ =
    case type_ of
        Var _ ->
            False

        Function input output ->
            [ input, output ]
                |> List.all isNotParametric

        List element ->
            element |> isNotParametric

        _ ->
            True


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

        List param ->
            "List " ++ toString param

        Unit ->
            "()"
