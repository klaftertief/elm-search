module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Lamdera
import Search.Model
import Search.View
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , queryString = ""
      , searchResult = []
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        EnteredSearchInput search ->
            ( { model | queryString = search }, Cmd.none )

        SubmittedSearch ->
            let
                maybeQuery =
                    model.queryString
                        |> Search.Model.queryListFromString
                        |> List.head
            in
            case maybeQuery of
                Just query ->
                    ( model, Lamdera.sendToBackend (QueryToBackend query) )

                Nothing ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SearchResultToFrontend chunks ->
            ( { model | searchResult = chunks }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "elm-search"
    , body =
        [ viewSearchForm model
        , viewSearchResult model
        ]
    }


viewSearchForm : Model -> Html FrontendMsg
viewSearchForm model =
    Html.form
        [ Html.Events.onSubmit SubmittedSearch
        ]
        [ Html.input
            [ Html.Attributes.type_ "search"
            , Html.Events.onInput EnteredSearchInput
            , Html.Attributes.value model.queryString
            , Html.Attributes.autofocus True
            ]
            []
        , Html.button
            [ Html.Attributes.type_ "submit"
            ]
            [ Html.text "Search" ]
        ]


viewSearchResult : Model -> Html FrontendMsg
viewSearchResult model =
    Html.div []
        (List.map Search.View.viewChunk model.searchResult)
