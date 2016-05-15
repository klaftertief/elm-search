module Search exposing (..)

-- where

import Html.App as Html
import Web.Model as Search
import Web.Update as Search
import Web.View as Search


main : Program Never
main =
    Html.program
        { init = Search.init
        , view = Search.view
        , update = Search.update
        , subscriptions = \_ -> Sub.none
        }
