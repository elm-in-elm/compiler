module Stage.InferTypes.Environment exposing
    ( Environment
    , add
    , empty
    , singleton
    )

{-| TODO Maybe a more descriptive name might be "DeclarationTypes" or
"DeclarationIds"?
-}

import Dict exposing (Dict)
import Elm.Data.ModuleName exposing (ModuleName)
import Elm.Data.Qualifiedness exposing (Qualified)
import Elm.Data.Type exposing (TypeOrId)
import Elm.Data.VarName exposing (VarName)


{-| A dict from variable names to their types/type IDs.

Usually one of these will be something of substance (the declaration of a var)
and the rest will be just type IDs of its usages. That way we can link them
together.

-}
type alias Environment =
    Dict ( ModuleName, VarName ) (List (TypeOrId Qualified))


empty : Environment
empty =
    Dict.empty


add : { module_ : ModuleName, name : VarName } -> TypeOrId Qualified -> Environment -> Environment
add { module_, name } type_ env =
    env
        |> Dict.update
            ( module_, name )
            (\maybeTypes ->
                case maybeTypes of
                    Nothing ->
                        Just [ type_ ]

                    Just types ->
                        Just (type_ :: types)
            )


singleton : { module_ : ModuleName, name : VarName } -> TypeOrId Qualified -> Environment
singleton name type_ =
    empty |> add name type_
