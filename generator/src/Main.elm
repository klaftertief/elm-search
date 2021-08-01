port module Main exposing (main)

import Docs.Search
import Http
import Json.Decode


type Model
    = Model


type Msg
    = GotAllPackagesResponse (Result Http.Error (List Docs.Search.Entry))


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
    , Cmd.batch
        [ log "Generator"
        , getAllPackages
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAllPackagesResponse (Err _) ->
            ( model
            , log "Got packages error"
            )

        GotAllPackagesResponse (Ok packages) ->
            ( model
            , log ("Got packages success: " ++ String.fromInt (List.length packages))
            )


getAllPackages : Cmd Msg
getAllPackages =
    Http.get
        { url = "https://package.elm-lang.org/search.json"
        , expect =
            Http.expectJson GotAllPackagesResponse
                (Json.Decode.list Docs.Search.decoder)
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port log : String -> Cmd msg
