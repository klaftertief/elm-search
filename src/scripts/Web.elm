module Web exposing (..)

import Html.App as Html
import Ports
import Web.Model as Search exposing (..)
import Web.Update as Search
import Web.View as Search


main : Program Flags
main =
    Html.programWithFlags
        { init = Search.init
        , view = Search.view
        , update = Search.update
        , subscriptions =
            \_ ->
                Sub.batch [ Ports.query LocationSearchChange ]
        }
