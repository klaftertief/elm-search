module Frontend.Page.Search exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

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
import Markdown
import Route


type Model
    = Model
        { session : Session
        , searchInput : Maybe String
        , searchResult : List Index.Block
        }


type Msg
    = EnteredSearchInput String
    | TriggeredSearch
    | GotSearchResult (Result Http.Error (List Index.Block))


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session maybeSearchInput =
    ( Model
        { session = session
        , searchInput = maybeSearchInput
        , searchResult = []
        }
    , maybeSearchInput
        |> Maybe.map
            (\query ->
                Http.get
                    { url = "http://localhost:3333/search?q=" ++ query
                    , expect = Http.expectJson GotSearchResult searchResultDecoder
                    }
            )
        |> Maybe.withDefault Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        EnteredSearchInput query ->
            ( Model { model | searchInput = Just query }
            , Cmd.none
            )

        TriggeredSearch ->
            ( Model model
            , case model.searchInput of
                Just query ->
                    Cmd.batch
                        [ Http.get
                            { url = "http://localhost:3333/search?q=" ++ query
                            , expect = Http.expectJson GotSearchResult searchResultDecoder
                            }
                        , Route.pushUrl (Session.navKey model.session)
                            (Route.Search <| Just query)
                        ]

                Nothing ->
                    Cmd.none
            )

        GotSearchResult (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            ( Model model, Cmd.none )

        GotSearchResult (Ok blocks) ->
            let
                _ =
                    Debug.log "# results" (List.length blocks)
            in
            ( Model { model | searchResult = blocks }, Cmd.none )


searchResultDecoder : Json.Decode.Decoder (List Index.Block)
searchResultDecoder =
    Json.Decode.field "result" (Json.Decode.list Json.Decode.value)
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
                        (List.map viewSearchResultBlock result)
                )
                model.searchResult
            ]
        ]


viewSearchResultBlock : Index.Block -> Html msg
viewSearchResultBlock block =
    case block of
        Index.Package package ->
            wrapBlock
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
    { code : List (Html msg)
    , identifier : List (Html msg)
    , comment : String
    }
    -> Html msg
wrapBlock { code, identifier, comment } =
    Html.div
        [ Html.Attributes.class "search-result-item" ]
        [ Html.p [] [ Html.pre [] [ Html.code [] code ] ]
        , Html.div [] [ Html.em [] identifier ]
        , Html.div []
            [ comment
                |> Markdown.toHtml Nothing
                |> List.head
                |> Maybe.withDefault (Html.text "")
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession (Model { session }) =
    session
