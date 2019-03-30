module Frontend.Page.Packages exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Frontend.Session as Session exposing (Session)
import Html exposing (Html)


type Model
    = Model Session


type Msg
    = Msg


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Elm Search - Packages"
    , body = Html.text "Elm Search - Packages"
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession (Model session) =
    session
