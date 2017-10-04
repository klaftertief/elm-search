module Web exposing (..)

import Docs.Package exposing (Package)
import Html
import Ports
import Web.Model as Search exposing (..)
import Web.Update as Search
import Web.View as Search


program : List Package -> Program Flags Model Msg
program packages =
    Html.programWithFlags
        { init = Search.init packages
        , view = Search.view
        , update = Search.update
        , subscriptions =
            \_ ->
                Sub.batch [ Ports.query LocationSearchChange ]
        }
