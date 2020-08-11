module Elm.Data.Import exposing (Import)

{-| The import statement.

      import Foo
      --> Import "Foo Nothing Nothing

      import Foo.Bar
      --> Import "Foo.Bar" Nothing Nothing

      import Foo as F
      --> Import "Foo" (Just "F") Nothing

      import Foo exposing (..)
      --> Import "Foo" Nothing (Just ExposingAll)

      import Foo as F exposing (..)
      --> Import "Foo" (Just "F") (Just ExposingAll)

@docs Import

-}

import Elm.Data.Comment exposing (Comment)
import Elm.Data.Exposing exposing (Exposing)
import Elm.Data.ModuleName exposing (ModuleName)


{-| -}
type alias Import =
    --{ moduleName : ModuleName
    --, as_ : Maybe ModuleName
    --, exposing_ : Maybe Exposing
    --}
    Commented


{-| Import with comments:

        {- commentsBefore -}
        import {- commentsBeforeModuleName -} Html.Attributes {- commentsAfterModuleName -} as {-commentsBeforeAs-} HtmlA {- commentsAfterAs -} exposing {- commentsBeforeExposing -} (..)

-}
type alias Commented =
    { commentsBefore : List Comment
    , commentsBeforeModuleName : List Comment
    , moduleName : ModuleName
    , commentsAfterModuleName : List Comment
    , as_ :
        Maybe
            { commentsBeforeAs : List Comment
            , as_ : ModuleName
            , commentsAfterAs : List Comment
            }
    , exposing_ :
        Maybe
            { commentsBeforeExposing : List Comment
            , exposing_ : Exposing
            }
    }



{- Create an [Import](#Import) from a [Commented](#Commented). -}
--fromCommented : Commented -> Import
--fromCommented { moduleName, as_, exposing_ } =
--    { moduleName = moduleName
--    , as_ = Maybe.map .as_ as_
--    , exposing_ = Maybe.map .exposing_ exposing_
--    }
