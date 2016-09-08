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
                , onInput SetFilterQueryString
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


viewSearchBody : Model -> Html Msg
viewSearchBody model =
    let
        searchBody =
            if String.isEmpty model.filter.lastQuery then
                viewSearchIntro
            else
                viewSearchResults model
    in
        div [ class "searchBody" ]
            [ searchBody ]


viewSearchIntro : Html Msg
viewSearchIntro =
    let
        exampleQueries =
            [ "map"
            , "(a -> b -> b) -> b -> List a -> b"
            , "Result x a -> (a -> Result x b) -> Result x b"
            , "String -> Int"
            ]

        exampleSearchItem query =
            li []
                [ a
                    [ style [ ( "cursor", "pointer" ) ]
                    , onClick (SetFilterQueryStringAndRunFilter query)
                    ]
                    [ text query ]
                ]
    in
        div [ class "searchIntro" ]
            [ h1 [] [ text "Welcome to Elm Search" ]
            , p [] [ text "Search the modules of the latest Elm packages by either function name or by approximate type signature." ]
            , h2 [] [ text "Example queries" ]
            , ul [] (List.map exampleSearchItem exampleQueries)
            ]


viewSearchResults : Model -> Html Msg
viewSearchResults { filter, result } =
    let
        viewQuery =
            div [ class "searchQuery" ]
                [ text <| "Showing results for: "
                , b [] [ text filter.lastQuery ]
                ]

        viewChunks =
            if not <| List.isEmpty result.chunks then
                List.map viewChunk result.chunks
            else
                [ p [] [ text "No Results Found." ] ]
    in
        div [ class "searchResult" ]
            (viewQuery :: viewChunks)


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
