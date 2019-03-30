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
import Elm.Search.Result
import Elm.Version
import Frontend.Route as Route
import Frontend.Session as Session exposing (Session)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Markdown


type Model
    = Model
        { session : Session
        , searchInput : Maybe String
        , searchResult : List Elm.Search.Result.Block
        }


type Msg
    = EnteredSearchInput String
    | TriggeredSearch
    | GotSearchResult (Result Http.Error (List Elm.Search.Result.Block))


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session maybeSearchInput =
    ( Model
        { session = session
        , searchInput = maybeSearchInput
        , searchResult = []
        }
    , Cmd.none
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
            ( Model { model | searchResult = blocks }, Cmd.none )


searchResultDecoder : Json.Decode.Decoder (List Elm.Search.Result.Block)
searchResultDecoder =
    Json.Decode.field "result"
        (Json.Decode.list Elm.Search.Result.blockDecoder)


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Elm Search"
    , body = viewContent model
    }


viewContent : Model -> Html Msg
viewContent (Model model) =
    Html.div []
        [ Html.input
            [ Html.Attributes.placeholder "Search"
            , Html.Attributes.value (model.searchInput |> Maybe.withDefault "")
            , Html.Events.onInput EnteredSearchInput
            ]
            []
        , Html.div []
            (List.map viewSearchResultBlock model.searchResult)
        ]


viewSearchResultBlock : Elm.Search.Result.Block -> Html msg
viewSearchResultBlock block =
    case block of
        Elm.Search.Result.Package _ _ ->
            Html.div [] [ Html.text "TODO" ]

        Elm.Search.Result.Module _ _ _ ->
            Html.div [] [ Html.text "TODO" ]

        Elm.Search.Result.Union packageIdentifier moduleIdentifier union ->
            wrapBlock
                { code =
                    [ Html.text "type "
                    , Html.strong []
                        [ Html.text union.name
                        , Html.text (" " ++ String.join " " union.args)
                        ]
                    , Html.text
                        (if List.isEmpty union.tags then
                            ""

                         else
                            " = "
                        )
                    , union.tags
                        |> List.map (\( name, tipes ) -> String.join " " (name :: List.map (Elm.Search.Result.elmTypeToString False) tipes))
                        |> String.join " | "
                        |> Html.text
                    ]
                , identifier =
                    [ Html.text
                        (String.join "/"
                            [ Elm.Package.toString packageIdentifier.name
                            , Elm.Version.toString packageIdentifier.version
                            , moduleIdentifier.name
                            ]
                        )
                    ]
                , comment = union.comment
                }

        Elm.Search.Result.Alias packageIdentifier moduleIdentifier alias_ ->
            wrapBlock
                { code =
                    [ Html.text "type alias "
                    , Html.strong []
                        [ Html.text alias_.name
                        , Html.text (" " ++ String.join " " alias_.args)
                        ]
                    , Html.text (" = " ++ Elm.Search.Result.elmTypeToString False alias_.tipe)
                    ]
                , identifier =
                    [ Html.text
                        (String.join "/"
                            [ Elm.Package.toString packageIdentifier.name
                            , Elm.Version.toString packageIdentifier.version
                            , moduleIdentifier.name
                            ]
                        )
                    ]
                , comment = alias_.comment
                }

        Elm.Search.Result.Value packageIdentifier moduleIdentifier value ->
            wrapBlock
                { code =
                    [ Html.strong [] [ Html.text value.name ]
                    , Html.text (" : " ++ Elm.Search.Result.elmTypeToString False value.tipe)
                    ]
                , identifier =
                    [ Html.text
                        (String.join "/"
                            [ Elm.Package.toString packageIdentifier.name
                            , Elm.Version.toString packageIdentifier.version
                            , moduleIdentifier.name
                            ]
                        )
                    ]
                , comment = value.comment
                }

        Elm.Search.Result.Binop packageIdentifier moduleIdentifier binop ->
            wrapBlock
                { code =
                    [ Html.strong [] [ Html.text ("(" ++ binop.name ++ ")") ]
                    , Html.text (" : " ++ Elm.Search.Result.elmTypeToString False binop.tipe)
                    ]
                , identifier =
                    [ Html.text
                        (String.join "/"
                            [ Elm.Package.toString packageIdentifier.name
                            , Elm.Version.toString packageIdentifier.version
                            , moduleIdentifier.name
                            ]
                        )
                    ]
                , comment = binop.comment
                }


wrapBlock :
    { code : List (Html msg)
    , identifier : List (Html msg)
    , comment : String
    }
    -> Html msg
wrapBlock { code, identifier, comment } =
    Html.div
        [ Html.Attributes.style "margin" "3rem 1rem" ]
        [ Html.p [] [ Html.pre [] [ Html.code [] code ] ]
        , Html.div [] [ Html.em [] identifier ]
        , Html.div []
            (comment
                |> String.split "\n\n"
                |> List.head
                |> Maybe.withDefault ""
                |> Markdown.toHtml Nothing
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession (Model { session }) =
    session
