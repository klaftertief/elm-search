port module Cli exposing (..)

import Docs.Package as Package exposing (Package)
import Html
import Html.App as Html
import Json.Decode as Decode exposing (Value)
import Search.Model as Search


type alias Flags =
    { index : Value }


type alias Model =
    { count : Int
    , index : Search.Index
    }


type Msg
    = NoOp


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
    ( { count = Debug.log "initial count" 0
      , index = buildIndex index
      }
    , Cmd.none
    )


main : Program Flags
main =
    Html.programWithFlags
        { init = init
        , view = (\model -> Html.text "")
        , update =
            (\msg model ->
                ( { model
                    | count = Debug.log "updated count" (model.count + 1)
                  }
                , Cmd.none
                )
            )
        , subscriptions =
            \_ ->
                Sub.batch [ foo (always NoOp) ]
        }


port foo : (String -> msg) -> Sub msg
