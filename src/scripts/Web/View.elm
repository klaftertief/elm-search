module Web.View exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Search.Model as Search
import Search.View as Search
import Web.Model as Model exposing (..)


view : Model -> Html Msg
view model =
    case model of
        Loading filter ->
            let
                search =
                    Search.initialModel
            in
                viewLoading { search | filter = filter }

        Failed error ->
            viewError (toString error)

        Ready search ->
            viewReady search


viewLoading : Search.Model -> Html Msg
viewLoading search =
    App.map SearchMsg
        <| div [ class "searchLoading" ]
            [ Search.viewSearchHeader search
            , viewStatus "Loading and indexing package docs..."
            ]


viewError : String -> Html Msg
viewError error =
    div [ class "searchError" ]
        [ viewStatus error ]


viewStatus : String -> Html msg
viewStatus status =
    p [ class "searchStatus" ] [ text status ]


viewReady : Search.Model -> Html Msg
viewReady search =
    App.map SearchMsg
        <| div [ class "searchReady" ]
            [ Search.viewSearchHeader search
            , Search.viewSearchResults search
            ]
