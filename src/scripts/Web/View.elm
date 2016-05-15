module Web.View exposing (..)

-- where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Web.Model as Model exposing (..)


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            viewLoading

        Failed error ->
            viewError (toString error)

        Success info ->
            viewSearch info


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading package docs..." ]


viewError : String -> Html Msg
viewError error =
    div [] [ text error ]


viewSearch : Info -> Html Msg
viewSearch info =
    let
        entries =
            if info.query == "" then
                []
            else
                info.packages
                    |> List.concatMap .modules
                    |> List.concatMap .entries
                    |> List.filter (.name >> String.contains info.query)
    in
        div []
            [ input [ onInput Query, value info.query ] []
            , div [] (entries |> List.map (\entry -> div [] [ text (toString entry.name) ]))
            ]
