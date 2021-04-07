module Search.View exposing
    ( Context(..)
    , annotation
    , annotationBlock
    , annotationName
    , longFunctionAnnotation
    , typeLength
    , viewChunk
    , viewField
    , viewType
    )

import Docs.Type as Type exposing (Type)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events as Events
import Search.Chunk as Chunk exposing (Chunk)
import String
import Tailwind.Utilities as Tailwind
import Utils.Code as Code
import Utils.Markdown as Markdown


viewChunk : Chunk -> Html msg
viewChunk chunk =
    div
        [ Attributes.css
            [ Tailwind.m_4
            , Tailwind.border
            , Tailwind.bg_gray_100
            , Tailwind.border_gray_200
            , Tailwind.rounded
            , Tailwind.shadow_sm
            , Tailwind.overflow_hidden
            ]
        ]
        [ div
            [ Attributes.css
                [ Tailwind.py_4
                , Tailwind.overflow_x_auto
                ]
            ]
            [ annotationBlock (annotation chunk) ]
        , div
            [ Attributes.css
                [ Tailwind.flex
                , Tailwind.justify_between
                , Tailwind.p_4
                , Tailwind.pt_0
                ]
            ]
            [ a
                [ href (Chunk.pathToPackage chunk.context)
                ]
                [ text (Chunk.identifierHome chunk.context) ]
            , a
                [ href (Chunk.pathToModule chunk.context)
                ]
                [ text chunk.context.moduleName ]
            ]
        , div
            [ Attributes.css
                [ Tailwind.hidden
                , Tailwind.p_4
                ]
            ]
            [ Maybe.map (Markdown.block >> Html.fromUnstyled) chunk.docs
                |> Maybe.withDefault (text "---")
            ]
        ]


annotationBlock : List (List (Html msg)) -> Html msg
annotationBlock bits =
    pre [] [ code [] (List.concat (List.intersperse [ text "\n" ] bits)) ]


annotation : Chunk -> List (List (Html msg))
annotation chunk =
    case chunk.tipe of
        Type.Function args result ->
            if String.length chunk.context.name + 3 + typeLength Other chunk.tipe > 128 then
                [ [ Html.span
                        [ Attributes.css
                            [ Tailwind.p_4
                            , Tailwind.pr_0
                            , Tailwind.sticky
                            , Tailwind.left_0
                            , Tailwind.bg_gray_100
                            ]
                        ]
                        (annotationName chunk
                            :: Code.padded Code.colon
                        )
                  ]
                ]
                    ++ longFunctionAnnotation args result

            else
                [ [ Html.span
                        [ Attributes.css
                            [ Tailwind.p_4
                            , Tailwind.pr_0
                            , Tailwind.sticky
                            , Tailwind.left_0
                            , Tailwind.bg_gray_100
                            ]
                        ]
                        (annotationName chunk
                            :: Code.padded Code.colon
                        )
                  ]
                    ++ viewType Other chunk.tipe
                ]

        _ ->
            [ annotationName chunk :: Code.padded Code.colon ++ viewType Other chunk.tipe ]


annotationName : Chunk -> Html msg
annotationName { context } =
    a
        [ href (Chunk.pathToValue context)
        ]
        [ text context.name ]


longFunctionAnnotation : List Type -> Type -> List (List (Html msg))
longFunctionAnnotation args result =
    let
        tipeHtml =
            List.map (viewType Func) (args ++ [ result ])

        starters =
            [ text "    " ]
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
                    List.concatMap
                        (\arg ->
                            viewType Func arg
                                ++ Code.padded Code.arrow
                        )
                        args
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
