module Search.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Logo
import Docs.Type as Type exposing (Type)
import Search.Chunk as Chunk exposing (Chunk)
import Search.Model as Model exposing (..)
import String
import Utils.Code exposing (arrow, colon, equals, keyword, padded, space)
import Utils.Markdown as Markdown


viewSearchHeader : Model -> Html Msg
viewSearchHeader search =
    div [ class "searchHeader" ]
        [ viewSearchBranding
        , viewSearchForm search
        ]


viewSearchBranding : Html Msg
viewSearchBranding =
    div [ class "searchBranding" ]
        [ viewLogo
        , span [ class "searchTitle" ] [ text "Elm Search" ]
        ]


viewLogo : Html msg
viewLogo =
    span [ class "searchLogo" ]
        [ Logo.viewWithSize 64 ]


viewSearchForm : Model -> Html Msg
viewSearchForm { filter, index, result } =
    let
        isDisabled =
            List.isEmpty index.chunks
    in
        Html.form
            [ classList
                [ ( "searchForm", True )
                , ( "searchFormDisabled", isDisabled )
                ]
            , action "."
            , onSubmit RunFilter
            ]
            [ input
                [ name "q"
                , type' "search"
                , onInput SetFilterQueryFrom
                , value filter.queryString
                , disabled isDisabled
                ]
                []
            , button
                [ type' "submit"
                , disabled isDisabled
                ]
                [ text "Search" ]
            ]


viewSearchResults : Model -> Html Msg
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
                [ a [ href (Chunk.pathToPackage chunk.context) ]
                    [ text (Chunk.identifierHome chunk.context) ]
                ]
            , div [ class "chunkModule" ]
                [ a [ href (Chunk.pathToModule chunk.context) ]
                    [ text (chunk.context.moduleName) ]
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
            if String.length chunk.context.name + 3 + Type.length Type.Other chunk.tipe > 64 then
                [ annotationName chunk ] :: longFunctionAnnotation args result
            else
                [ annotationName chunk :: padded colon ++ Type.toHtml Type.Other chunk.tipe ]

        _ ->
            [ annotationName chunk :: padded colon ++ Type.toHtml Type.Other chunk.tipe ]


annotationName : Chunk -> Html msg
annotationName { context } =
    a [ href (Chunk.pathToValue context) ]
        [ text context.name ]


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
