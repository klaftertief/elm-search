port module Cli exposing (..)

import Docs.Package as Package exposing (Package)
import Html
import Html.App as Html
import Json.Decode as Decode exposing (Value)
import Search.Model as Search


type alias Flags =
    { index : Value }


type alias Model =
    { index : Search.Index }


type alias Response =
    { query : String
    , result : Value
    }


type Msg
    = Search String


decodeSafe : Decode.Decoder (List (Maybe Package))
decodeSafe =
    [ Decode.map Just Package.decoder, Decode.succeed Nothing ]
        |> Decode.oneOf
        |> Decode.list


buildIndex : Value -> Search.Index
buildIndex value =
    Decode.decodeValue decodeSafe value
        |> Result.toMaybe
        |> Maybe.withDefault []
        |> List.filterMap identity
        |> Search.buildIndex


init : Flags -> ( Model, Cmd Msg )
init { index } =
    ( { index = buildIndex index }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search query ->
            let
                filter =
                    Search.Filter query
                        (Search.queryListFromString query)

                result =
                    Search.runFilter filter model.index
            in
                ( model
                , response (Response query (Search.encodeResult result))
                )


main : Program Flags
main =
    Html.programWithFlags
        { init = init
        , view = (\model -> Html.text "")
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch [ request Search ]
        }


port request : (String -> msg) -> Sub msg


port response : Response -> Cmd msg
