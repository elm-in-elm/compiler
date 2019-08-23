module Stage.InferTypes.IdSource exposing
    ( IdSource
    , empty
    , increment
    , one
    )

{-| A boilerplate-reducing abstraction for the `assignIds` stage of InferTypes stage.

Allows the user to not think about incrementing IDs after every subexpression.

-}


type IdSource
    = IdSource Int


empty : IdSource
empty =
    IdSource 0


increment : IdSource -> ( Int, IdSource )
increment (IdSource currentId) =
    ( currentId
    , IdSource (currentId + 1)
    )


one : IdSource -> (Int -> b) -> ( b, IdSource )
one idSource fn =
    let
        ( currentId, nextSource ) =
            increment idSource
    in
    ( fn currentId
    , nextSource
    )
