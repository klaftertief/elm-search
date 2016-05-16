module Web.View exposing (..)

-- where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Package.Module.Type as Type
import Search.Distance as Distance
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
        weightedChunks =
            case Type.parse info.query of
                Ok tipe ->
                    case tipe of
                        Type.Var _ ->
                            info.chunks
                                |> List.map (\chunk -> ( Distance.name info.query chunk, chunk ))

                        _ ->
                            info.chunks
                                |> List.map (\entry -> ( Distance.tipe tipe entry, entry ))

                Err _ ->
                    []

        filteredChunks =
            weightedChunks
                |> List.filter (\( distance, _ ) -> distance <= Distance.lowPenalty)
                |> List.sortBy fst
                |> List.map snd
    in
        div []
            [ input [ onInput Query, value info.query ] []
            , div [] (filteredChunks |> List.map (\chunk -> div [] [ text (chunk.name ++ " : " ++ toString chunk.tipe) ]))
            ]
