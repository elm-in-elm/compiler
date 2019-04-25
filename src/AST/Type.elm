module AST.Type exposing (Type(..))

{-| In its own module because both Error.TypeError and AST.Typechecked need to see it
-}


type Type
    = TVar Int
    | TInt
    | TFunction Type Type
