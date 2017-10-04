module Web.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Search.Model as Search
import Search.View as Search
import Web.Model as Model exposing (..)


view : Model -> Html Msg
view (Ready search) =
    Html.map SearchMsg <|
        div [ class "searchReady" ]
            [ Search.viewSearchHeader search
            , Search.viewSearchBody search
            ]
