module Frontend exposing (main)

import Browser
import Browser.Navigation as Nav
import Elm.Search.Result
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Url



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
    { key : Nav.Key
    , url : Url.Url
    , searchInput : String
    , searchResult : List Elm.Search.Result.Block
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , searchInput = ""
      , searchResult = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | EnteredSearchInput String
    | GotSearchResult (Result Http.Error (List Elm.Search.Result.Block))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        EnteredSearchInput query ->
            ( { model | searchInput = query }
            , Http.get
                { url = "http://localhost:3333/search?q=" ++ query
                , expect = Http.expectJson GotSearchResult searchResultDecoder
                }
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
        [ input [ placeholder "Search", onInput EnteredSearchInput ] []
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

        Elm.Search.Result.Union _ _ union ->
            Html.div []
                [ Html.text ("type " ++ union.name)
                ]

        Elm.Search.Result.Alias _ _ alias_ ->
            Html.div []
                [ Html.text ("type alias " ++ alias_.name ++ " = " ++ Elm.Search.Result.elmTypeToString False alias_.tipe)
                ]

        Elm.Search.Result.Value _ _ value ->
            Html.div []
                [ Html.text (value.name ++ " : " ++ Elm.Search.Result.elmTypeToString False value.tipe)
                ]

        Elm.Search.Result.Binop _ _ binop ->
            Html.div []
                [ Html.text ("(" ++ binop.name ++ ") : " ++ Elm.Search.Result.elmTypeToString False binop.tipe)
                ]
