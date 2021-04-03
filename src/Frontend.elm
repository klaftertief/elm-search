module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import FeatherIcons
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


init : Url.Url -> Browser.Navigation.Key -> ( Model, Cmd FrontendMsg )
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
                    , Cmd.batch [ Browser.Navigation.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
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
        [ viewStyles
        , viewHeading
        , viewSearchForm model
        , viewSearchResult model
        ]
    }


viewStyles : Html msg
viewStyles =
    Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href "/main.css"
        ]
        []


viewHeading : Html msg
viewHeading =
    Html.header
        [ Html.Attributes.class "p-4 flex justify-center"
        ]
        [ Html.p []
            [ Html.text "elm-search" ]
        ]


viewSearchForm : Model -> Html FrontendMsg
viewSearchForm model =
    Html.form
        [ Html.Events.onSubmit SubmittedSearch
        , Html.Attributes.class "p-4 flex justify-center sticky top-0 bg-white"
        ]
        [ Html.input
            [ Html.Attributes.class "block rounded-l-md bg-gray-200 border-transparent flex-1"
            , Html.Attributes.class "focus:border-transparent focus:outline-none focus:ring-0 focus:bg-gray-300"
            , Html.Attributes.type_ "search"
            , Html.Events.onInput EnteredSearchInput
            , Html.Attributes.value model.queryString
            , Html.Attributes.autofocus True
            ]
            []
        , Html.button
            [ Html.Attributes.class "px-4 rounded-r-md bg-gray-200"
            , Html.Attributes.type_ "submit"
            ]
            [ FeatherIcons.search
                |> FeatherIcons.toHtml []
            ]
        ]


viewSearchResult : Model -> Html FrontendMsg
viewSearchResult model =
    Html.div []
        (List.map Search.View.viewChunk model.searchResult)
