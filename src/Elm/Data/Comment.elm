module Elm.Data.Comment exposing (Comment, CommentType(..))

import Elm.Data.Located exposing (Located)


{-| Comment information

@docs Comment, CommentType

-}
type alias Comment =
    { content : Located String
    , type_ : CommentType
    }


type CommentType
    = SingleLine
    | MultiLine
