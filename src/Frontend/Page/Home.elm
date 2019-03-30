module Frontend.Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Frontend.Route as Route
import Frontend.Session as Session exposing (Session)
import Html exposing (Html)
import Html.Attributes
import Html.Events


type Model
    = Model
        { session : Session
        , searchInput : String
        }


type Msg
    = EnteredSearchInput String
    | TriggeredSearch


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model
        { session = session
        , searchInput = ""
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        EnteredSearchInput query ->
            ( Model { model | searchInput = query }
            , Cmd.none
            )

        TriggeredSearch ->
            ( Model model
            , Route.pushUrl (Session.navKey model.session)
                (Route.Search <| Just model.searchInput)
            )


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Elm Search"
    , body = viewContent model
    }


viewContent : Model -> Html Msg
viewContent (Model model) =
    Html.input
        [ Html.Attributes.placeholder "Search"
        , Html.Attributes.value model.searchInput
        , Html.Events.onInput EnteredSearchInput
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession (Model { session }) =
    session
