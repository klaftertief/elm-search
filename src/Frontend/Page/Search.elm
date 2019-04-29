module Frontend.Page.Search exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Browser.Dom as Dom
import Elm.Package
import Elm.Search.Index as Index
import Elm.Search.Query as SearchQuery
import Elm.Version
import Frontend.Session as Session exposing (Session)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Http
import Json.Decode
import Markdown.Block
import Markdown.Inline
import Route
import SyntaxHighlight
import Task


type Model
    = Model
        { session : Session
        , searchInput : Maybe String
        , searchResult : List Index.Block
        , focusedBlockIdentifier : Maybe String
        }


type Msg
    = Forget
    | EnteredSearchInput String
    | TriggeredSearch
    | GotSearchResult (Result Http.Error (List Index.Block))
    | TriggeredFocusSelect String
    | TriggeredFocusDeselect String


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session maybeSearchInput =
    ( Model
        { session = session
        , searchInput = maybeSearchInput
        , searchResult = []
        , focusedBlockIdentifier = Nothing
        }
    , maybeSearchInput
        |> Maybe.map
            (\query ->
                Http.get
                    { url = "/api/search?q=" ++ query
                    , expect = Http.expectJson GotSearchResult searchResultDecoder
                    }
            )
        |> Maybe.withDefault Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Forget ->
            ( Model model
            , Cmd.none
            )

        EnteredSearchInput query ->
            ( Model { model | searchInput = Just query }
            , Cmd.none
            )

        TriggeredSearch ->
            ( Model model
            , case model.searchInput of
                Just query ->
                    Cmd.batch
                        -- [ Http.get
                        --     { url = "/api/search?q=" ++ query
                        --     , expect = Http.expectJson GotSearchResult searchResultDecoder
                        --     }
                        [ Route.pushUrl (Session.navKey model.session)
                            (Route.Search <| Just query)
                        ]

                Nothing ->
                    Cmd.none
            )

        GotSearchResult (Err err) ->
            ( Model model, Cmd.none )

        GotSearchResult (Ok blocks) ->
            ( Model { model | searchResult = blocks }, Cmd.none )

        TriggeredFocusSelect identifier ->
            ( Model { model | focusedBlockIdentifier = Just identifier }
              -- , Dom.getElement identifier
              --     |> Task.andThen (\info -> Dom.setViewport 0 info.element.y)
              --     |> Task.attempt (\_ -> Forget)
            , Cmd.none
            )

        TriggeredFocusDeselect _ ->
            ( Model { model | focusedBlockIdentifier = Nothing }, Cmd.none )


searchResultDecoder : Json.Decode.Decoder (List Index.Block)
searchResultDecoder =
    Json.Decode.field "response" (Json.Decode.list Json.Decode.value)
        |> Json.Decode.map (List.map (Json.Decode.decodeValue Index.blockDecoder >> Result.toMaybe))
        |> Json.Decode.map (List.filterMap identity)


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Elm Search"
    , body = viewContent model
    }


viewContent : Model -> Html Msg
viewContent (Model model) =
    Html.div [ Html.Attributes.class "page-search" ]
        [ Html.header []
            [ Html.form [ Html.Events.onSubmit TriggeredSearch ]
                [ Html.input
                    [ Html.Attributes.placeholder "(a -> b) -> Maybe a -> Maybe b"
                    , Html.Attributes.value (model.searchInput |> Maybe.withDefault "")
                    , Html.Events.onInput EnteredSearchInput
                    ]
                    []
                ]
            ]
        , Html.main_ []
            [ Html.Lazy.lazy
                (\result ->
                    Html.div [ Html.Attributes.class "search-result" ]
                        (List.map (viewSearchResultBlock model.focusedBlockIdentifier) result)
                )
                model.searchResult
            ]
        ]


viewSearchResultBlock : Maybe String -> Index.Block -> Html Msg
viewSearchResultBlock focusedIdentifier block =
    case block of
        Index.Package package ->
            wrapBlock
                (Index.packageIdentifierToString package.identifier)
                focusedIdentifier
                { code =
                    [ Html.text "package "
                    , Html.strong []
                        [ Html.text (Index.packageIdentifierToString package.identifier)
                        ]
                    ]
                , identifier =
                    [ Html.text (Index.packageIdentifierToString package.identifier)
                    ]
                , comment = package.info.summary
                }

        Index.Module module_ ->
            wrapBlock
                (Index.moduleIdentifierToString module_.identifier)
                focusedIdentifier
                { code =
                    [ Html.text "module "
                    , Html.strong []
                        [ Html.text (Index.moduleIdentifierToString module_.identifier)
                        ]
                    ]
                , identifier =
                    [ Html.text (Index.moduleIdentifierToString module_.identifier)
                    ]
                , comment = module_.info.comment
                }

        Index.Union union ->
            wrapBlock
                (Index.exposedIdentifierToString union.identifier)
                focusedIdentifier
                { code =
                    [ Html.text "type "
                    , Html.strong []
                        [ Html.text union.info.name
                        , Html.text (" " ++ String.join " " union.info.args)
                        ]
                    , Html.text
                        (if List.isEmpty union.info.tags then
                            ""

                         else
                            " = "
                        )
                    , union.info.tags
                        |> List.map (\( name, tipes ) -> String.join " " (name :: List.map (Index.elmTypeToText False) tipes))
                        |> String.join " | "
                        |> Html.text
                    ]
                , identifier =
                    [ Html.text (Index.exposedIdentifierToString union.identifier)
                    ]
                , comment = union.info.comment
                }

        Index.Alias alias_ ->
            wrapBlock
                (Index.exposedIdentifierToString alias_.identifier)
                focusedIdentifier
                { code =
                    [ Html.text "type alias "
                    , Html.strong []
                        [ Html.text alias_.info.name
                        , Html.text (" " ++ String.join " " alias_.info.args)
                        ]
                    , Html.text (" = " ++ Index.elmTypeToText False alias_.info.tipe)
                    ]
                , identifier =
                    [ Html.text (Index.exposedIdentifierToString alias_.identifier)
                    ]
                , comment = alias_.info.comment
                }

        Index.Value value ->
            wrapBlock
                (Index.exposedIdentifierToString value.identifier)
                focusedIdentifier
                { code =
                    [ Html.strong [] [ Html.text value.info.name ]
                    , Html.text (" : " ++ Index.elmTypeToText False value.info.tipe)
                    ]
                , identifier =
                    [ Html.text (Index.exposedIdentifierToString value.identifier)
                    ]
                , comment = value.info.comment
                }

        Index.Binop binop ->
            wrapBlock
                (Index.exposedIdentifierToString binop.identifier)
                focusedIdentifier
                { code =
                    [ Html.strong [] [ Html.text ("(" ++ binop.info.name ++ ")") ]
                    , Html.text (" : " ++ Index.elmTypeToText False binop.info.tipe)
                    ]
                , identifier =
                    [ Html.text (Index.exposedIdentifierToString binop.identifier)
                    ]
                , comment = binop.info.comment
                }


wrapBlock :
    String
    -> Maybe String
    ->
        { code : List (Html Msg)
        , identifier : List (Html Msg)
        , comment : String
        }
    -> Html Msg
wrapBlock id focusedId { code, identifier, comment } =
    Html.div
        ([ Html.Attributes.id id
         , Html.Attributes.class "search-result-item"
         , Html.Attributes.tabindex 0
         ]
            ++ (if Just id == focusedId then
                    [ Html.Events.onClick (TriggeredFocusDeselect id)
                    , Html.Attributes.class "selected"
                    ]

                else
                    [ Html.Events.onClick (TriggeredFocusSelect id) ]
               )
        )
        [ Html.div
            [ Html.Attributes.class "search-result-item-header" ]
            [ Html.pre [] [ Html.code [] code ] ]
        , Html.div
            [ Html.Attributes.class "search-result-item-comment" ]
            (if Just id == focusedId then
                markdownToHtml comment

             else
                [ markdownToHtml comment
                    |> List.head
                    |> Maybe.withDefault (Html.text "")
                ]
            )
        , Html.div
            [ Html.Attributes.class "search-result-item-identifier" ]
            [ Html.em [] identifier ]
        ]


markdownToHtml : String -> List (Html msg)
markdownToHtml markdownString =
    markdownString
        |> Markdown.Block.parse Nothing
        |> List.map markdownBlockToHtml
        |> List.concat


markdownBlockToHtml : Markdown.Block.Block b i -> List (Html msg)
markdownBlockToHtml block =
    case block of
        Markdown.Block.CodeBlock _ code ->
            [ SyntaxHighlight.elm code
                |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
                |> Result.withDefault
                    (Html.pre []
                        [ Html.code [] [ Html.text code ] ]
                    )
            ]

        _ ->
            Markdown.Block.defaultHtml
                (Just markdownBlockToHtml)
                (Just markdownInlineToHtml)
                block


markdownInlineToHtml : Markdown.Inline.Inline i -> Html msg
markdownInlineToHtml inline =
    case inline of
        _ ->
            Markdown.Inline.defaultHtml (Just markdownInlineToHtml) inline


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession (Model { session }) =
    session
