module Frontend exposing (main)

import Browser
import Browser.Navigation
import Elm.Package
import Elm.Search.Result
import Elm.Version
import Frontend.Route as Route
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Markdown
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key
    , searchInput : String
    , searchResult : List Elm.Search.Result.Block
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    case Route.fromUrl url of
        Just (Route.Search (Just query)) ->
            ( { key = key
              , searchInput = query
              , searchResult = []
              }
            , Http.get
                { url = "http://localhost:3333/search?q=" ++ query
                , expect = Http.expectJson GotSearchResult searchResultDecoder
                }
            )

        Just (Route.Search _) ->
            ( { key = key
              , searchInput = ""
              , searchResult = []
              }
            , Cmd.none
            )

        Just Route.Home ->
            ( { key = key
              , searchInput = ""
              , searchResult = []
              }
            , Cmd.none
            )

        Just Route.Packages ->
            ( { key = key
              , searchInput = ""
              , searchResult = []
              }
            , Cmd.none
            )

        Nothing ->
            ( { key = key
              , searchInput = ""
              , searchResult = []
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | EnteredSearchInput String
    | GotSearchResult (Result Http.Error (List Elm.Search.Result.Block))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( model
            , Cmd.none
            )

        EnteredSearchInput query ->
            ( { model | searchInput = query }
            , Cmd.batch
                [ Http.get
                    { url = "http://localhost:3333/search?q=" ++ query
                    , expect = Http.expectJson GotSearchResult searchResultDecoder
                    }
                , Route.pushUrl model.key
                    (Route.Search <| Just query)
                ]
            )

        GotSearchResult (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        GotSearchResult (Ok blocks) ->
            ( { model | searchResult = blocks }, Cmd.none )


searchResultDecoder : Json.Decode.Decoder (List Elm.Search.Result.Block)
searchResultDecoder =
    Json.Decode.field "result"
        (Json.Decode.list Elm.Search.Result.blockDecoder)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Search"
    , body =
        [ input
            [ placeholder "Search"
            , value model.searchInput
            , onInput EnteredSearchInput
            ]
            []
        , Html.div []
            (List.map viewSearchResultBlock model.searchResult)
        ]
    }


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
