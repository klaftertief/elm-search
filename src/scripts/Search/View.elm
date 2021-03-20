module Search.View exposing (Context(..), annotation, annotationBlock, annotationName, longFunctionAnnotation, typeLength, viewChunk, viewField, viewLogo, viewSearchBody, viewSearchBranding, viewSearchForm, viewSearchHeader, viewSearchIntro, viewSearchResults, viewType)

import Docs.Type as Type exposing (Type)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Logo
import Search.Chunk as Chunk exposing (Chunk)
import Search.Model as Model exposing (..)
import String
import Utils.Code as Code
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
            , type_ "search"
            , onInput SetFilterQueryString
            , value filter.queryString
            , autofocus True
            ]
            []
        , button
            [ type_ "submit"
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
                    [ style "cursor" "pointer"
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


viewChunk : Chunk -> Html msg
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
                    [ text chunk.context.moduleName ]
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
            if String.length chunk.context.name + 3 + typeLength Other chunk.tipe > 64 then
                [ annotationName chunk ] :: longFunctionAnnotation args result

            else
                [ annotationName chunk :: Code.padded Code.colon ++ viewType Other chunk.tipe ]

        _ ->
            [ annotationName chunk :: Code.padded Code.colon ++ viewType Other chunk.tipe ]


annotationName : Chunk -> Html msg
annotationName { context } =
    a [ href (Chunk.pathToValue context) ]
        [ text context.name ]


longFunctionAnnotation : List Type -> Type -> List (List (Html msg))
longFunctionAnnotation args result =
    let
        tipeHtml =
            List.map (viewType Func) (args ++ [ result ])

        starters =
            [ text "    ", Code.colon, text "  " ]
                :: List.repeat (List.length args) [ text "    ", Code.arrow, Code.space ]
    in
    List.map2 (++) starters tipeHtml



-- TYPE


type Context
    = Func
    | App
    | Other


viewType : Context -> Type -> List (Html msg)
viewType context tipe =
    case tipe of
        Type.Function args result ->
            let
                maybeAddParens =
                    case context of
                        Func ->
                            Code.addParens

                        App ->
                            Code.addParens

                        Other ->
                            identity

                argsHtml =
                    List.concatMap (\arg -> viewType Func arg ++ Code.padded Code.arrow) args
            in
            maybeAddParens (argsHtml ++ viewType Func result)

        Type.Var name ->
            [ text name ]

        Type.Apply name [] ->
            [ text name.name ]

        Type.Apply name args ->
            let
                maybeAddParens =
                    case context of
                        Func ->
                            identity

                        App ->
                            Code.addParens

                        Other ->
                            identity

                argsHtml =
                    List.concatMap (\arg -> Code.space :: viewType App arg) args
            in
            maybeAddParens (text name.name :: argsHtml)

        Type.Tuple args ->
            List.map (viewType Other) args
                |> List.intersperse [ text ", " ]
                |> List.concat
                |> Code.addParens

        Type.Record fields ext ->
            let
                fieldsHtml =
                    List.map viewField fields
                        |> List.intersperse [ text ", " ]
                        |> List.concat

                recordInsides =
                    case ext of
                        Nothing ->
                            fieldsHtml

                        Just extName ->
                            text extName :: text " | " :: fieldsHtml
            in
            text "{ " :: recordInsides ++ [ text " }" ]


viewField : ( String, Type ) -> List (Html msg)
viewField ( field, tipe ) =
    text field :: Code.space :: Code.colon :: Code.space :: viewType Other tipe


typeLength : Context -> Type -> Int
typeLength context tipe =
    case tipe of
        Type.Function args result ->
            let
                parens =
                    case context of
                        Func ->
                            2

                        App ->
                            2

                        Other ->
                            0

                argLengths =
                    List.map (\t -> 4 + typeLength Func t) args
            in
            parens + List.sum argLengths + typeLength Func result

        Type.Var name ->
            String.length name

        Type.Apply { name } [] ->
            String.length name

        Type.Apply { name } args ->
            let
                parens =
                    case context of
                        Func ->
                            0

                        App ->
                            2

                        Other ->
                            0

                argsLength =
                    List.sum (List.map (\t -> 1 + typeLength App t) args)
            in
            parens + String.length name + argsLength

        Type.Tuple args ->
            List.sum (List.map (\t -> 2 + typeLength Other t) args)

        Type.Record fields ext ->
            let
                fieldLength ( field, tipe_ ) =
                    String.length field + 3 + typeLength Other tipe_

                recordLength =
                    2 + List.sum (List.map (\ft -> 2 + fieldLength ft) fields)

                extLength =
                    case ext of
                        Nothing ->
                            0

                        Just extName ->
                            2 + String.length extName
            in
            recordLength + extLength
