module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Css
import Css.Global
import FeatherIcons
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Lamdera
import Search.Model
import Search.View
import Tailwind.Utilities as Tailwind
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
            |> List.map Html.toUnstyled
    }


viewStyles : Html msg
viewStyles =
    Css.Global.global Tailwind.globalStyles


viewHeading : Html msg
viewHeading =
    Html.header
        [ Attributes.css
            [ Tailwind.p_4, Tailwind.flex, Tailwind.justify_center ]
        ]
        [ Html.p []
            [ Html.text "elm-search" ]
        ]


viewSearchForm : Model -> Html FrontendMsg
viewSearchForm model =
    Html.form
        [ Events.onSubmit SubmittedSearch
        , Attributes.css
            [ Tailwind.p_4
            , Tailwind.flex
            , Tailwind.justify_center
            , Tailwind.sticky
            , Tailwind.top_0
            , Tailwind.bg_white
            ]
        ]
        [ Html.input
            [ Attributes.css
                [ Tailwind.block
                , Tailwind.rounded_l_md
                , Tailwind.bg_gray_200
                , Tailwind.border_transparent
                , Tailwind.flex_1
                ]
            , Attributes.css
                [ Css.focus
                    [ Tailwind.border_transparent
                    , Tailwind.outline_none
                    , Tailwind.ring_0
                    , Tailwind.bg_gray_300
                    ]
                ]
            , Attributes.type_ "search"
            , Events.onInput EnteredSearchInput
            , Attributes.value model.queryString
            , Attributes.autofocus True
            ]
            []
        , Html.button
            [ Attributes.css
                [ Tailwind.px_4
                , Tailwind.rounded_r_md
                , Tailwind.bg_gray_200
                ]
            , Attributes.type_ "submit"
            ]
            [ FeatherIcons.search
                |> FeatherIcons.toHtml []
                |> Html.fromUnstyled
            ]
        ]


viewSearchResult : Model -> Html FrontendMsg
viewSearchResult model =
    Html.div []
        (List.map
            Search.View.viewChunk
            model.searchResult
        )
