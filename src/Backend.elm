module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Packages
import Search.Chunk
import Search.Model
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


index =
    { chunks =
        Packages.packages
            --|> List.take 10
            |> List.concatMap Search.Chunk.packageChunks
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { chunks = []
      }
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
        QueryToBackend query ->
            let
                result =
                    Search.Model.runFilter { queryString = "", query = [ query ], lastQuery = "" } index
                        |> .chunks
            in
            ( model
            , Lamdera.sendToFrontend clientId (SearchResultToFrontend result)
            )
