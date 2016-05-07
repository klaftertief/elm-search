module Web exposing (..)

-- where

import Html.App as Html
import Component.Search as Search


main : Program Never
main =
  Html.program
    { init = Search.init
    , view = Search.view
    , update = Search.update
    , subscriptions = \_ -> Sub.none
    }
