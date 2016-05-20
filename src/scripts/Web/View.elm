module Web.View exposing (..)

-- where

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Logo
import Package.Module.Type as Type exposing (Type)
import Package.Version as Version exposing (Version)
import Search.Chunk as Chunk exposing (Chunk)
import Search.Model as Search
import Set
import String
import Utils.Code exposing (arrow, colon, equals, keyword, padded, space)
import Utils.Markdown as Markdown
import Web.Model as Model exposing (..)


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            viewLoading

        Failed error ->
            viewError (toString error)

        Ready search ->
            viewSearch search


viewLoading : Html Msg
viewLoading =
    div [] [ text "Loading package docs..." ]


viewError : String -> Html Msg
viewError error =
    div [] [ text error ]


viewSearch : Search.Model -> Html Msg
viewSearch search =
    div []
        [ viewSearchHeader search
        , viewSearchResults search
        ]


viewSearchHeader : Search.Model -> Html Msg
viewSearchHeader search =
    div [ class "searchHeader" ]
        [ viewSearchTitle
        , viewSearchForm search
        ]


viewSearchTitle : Html Msg
viewSearchTitle =
    div [ class "searchBranding" ]
        [ viewLogo
        , span [ class "searchTitle" ] [ text "Elm Search" ]
        ]


viewLogo : Html msg
viewLogo =
    span [ class "searchLogo" ]
        [ Logo.viewWithSize 96 ]


viewSearchForm : Search.Model -> Html Msg
viewSearchForm { index, filter } =
    App.map SearchMsg
        <| Html.form
            [ class "searchForm"
            , action "."
            , onSubmit Search.RunFilter
            ]
            [ input
                [ name "q"
                , type' "search"
                , onInput Search.SetFilterQueryFrom
                , value filter.queryString
                ]
                []
            , label []
                [ select
                    [ name "v"
                    , on "change" (Decode.map Search.SetFilterVersionFrom targetValue)
                    ]
                    (option [] [ text "any" ]
                        :: (index.elmVersions
                                |> Set.toList
                                |> List.reverse
                                |> List.map (\vsn -> option [] [ text (Version.vsnToString vsn) ])
                           )
                    )
                ]
            , button [ type' "submit" ] [ text "Search" ]
            ]


viewSearchResults : Search.Model -> Html Msg
viewSearchResults { result } =
    div [ class "searchResult" ]
        (List.map viewChunk result.chunks)


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
            [ div [ class "chunkPath" ]
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
