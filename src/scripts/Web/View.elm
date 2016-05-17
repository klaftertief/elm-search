module Web.View exposing (..)

-- where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Package.Module.Type as Type
import Package.Module.Name as Name
import Search.Distance as Distance
import Search.Chunk as Chunk exposing (Chunk)
import Utils.Code exposing (arrow, colon, equals, keyword, padded, space)
import Utils.Markdown as Markdown
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
            [ div [ class "searchForm" ]
                [ input [ onInput Query, value info.query ] [] ]
            , div [ class "searchResult" ]
                (List.map viewChunk filteredChunks)
            ]


viewChunk : Chunk -> Html Msg
viewChunk chunk =
    div [ class "searchChunk" ]
        [ div [ class "chunkAnnotation" ]
            [ code []
                (text chunk.name :: padded colon ++ Type.toHtml Type.Other chunk.tipe)
            ]
        , div [ class "chunkDocumentation" ]
            [ case chunk.docs of
                Just docs ->
                    Markdown.block docs

                Nothing ->
                    text "---"
            ]
        , div [ class "chunkMeta" ]
            [ div [ class "chunkPackage" ] [ text chunk.packageIdentifier ]
            , div [ class "chunkModule" ] [ text (Name.nameToString chunk.moduleName) ]
            ]
        ]
