module Backend exposing (..)

import Index
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel


index =
    Index.index


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( {}
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        SearchQuerySubmitted query ->
            ( model
            , Lamdera.sendToFrontend clientId
                (SearchResultSent
                    (index
                        |> List.filterMap
                            (\package ->
                                if String.contains query package.project.name then
                                    [ package.project.user, package.project.name, package.project.version ]
                                        |> String.join "/"
                                        |> Just

                                else
                                    Nothing
                            )
                    )
                )
            )
