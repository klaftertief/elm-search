module Web.View exposing (..)

-- where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Package.Module.Type as Type exposing (Type)
import Package.Version as Version exposing (Version)
import Search.Distance as Distance
import Search.Chunk as Chunk exposing (Chunk)
import String
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
    div []
        [ Html.form [ class "searchForm", onSubmit SearchQuery ]
            [ input [ onInput SetQuery, value info.query ] [] ]
        , div [ class "searchResult" ]
            (List.map viewChunk info.filteredChunks)
        ]


viewChunk : Chunk -> Html Msg
viewChunk chunk =
    div [ class "searchChunk" ]
        [ div [ class "chunkAnnotation" ]
            [ annotationBlock (annotation chunk) ]
        , div [ class "chunkDocumentation" ]
            [ Maybe.map Markdown.block chunk.docs
                |> Maybe.withDefault (text "---")
            ]
        , div [ class "chunkMeta" ]
            [ div [ class "chunkPackageLink" ]
                [ a [ href (Chunk.pathTo chunk.name) ]
                    [ text (Chunk.identifierHome chunk.name) ]
                ]
            , div [ class "chunkVersion" ]
                [ text
                    (Maybe.map Version.vsnToString chunk.elmVersion
                        |> Maybe.withDefault "---"
                    )
                ]
            ]
        ]


annotationBlock : List (List (Html msg)) -> Html msg
annotationBlock bits =
    pre [] [ code [] (List.concat (List.intersperse [ text "\n" ] bits)) ]


annotation : Chunk -> List (List (Html msg))
annotation chunk =
    case chunk.tipe of
        Type.Function args result ->
            if String.length chunk.name.name + 3 + Type.length Type.Other chunk.tipe > 64 then
                [ annotationName chunk ] :: longFunctionAnnotation args result
            else
                [ annotationName chunk :: padded colon ++ Type.toHtml Type.Other chunk.tipe ]

        _ ->
            [ annotationName chunk :: padded colon ++ Type.toHtml Type.Other chunk.tipe ]


annotationName : Chunk -> Html msg
annotationName { name } =
    a [ href (Chunk.pathTo name) ]
        [ text name.name ]


longFunctionAnnotation : List Type -> Type -> List (List (Html msg))
longFunctionAnnotation args result =
    let
        tipeHtml =
            List.map (Type.toHtml Type.Func) (args ++ [ result ])

        starters =
            [ text "    ", colon, text "  " ]
                :: List.repeat (List.length args) [ text "    ", arrow, space ]
    in
        List.map2 (++) starters tipeHtml
