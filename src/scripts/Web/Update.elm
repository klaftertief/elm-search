module Web.Update exposing (..)

-- where

import Http
import Json.Decode as Decode
import Task
import Package.Package as Package
import Web.Model as Model exposing (..)


init : ( Model, Cmd Msg )
init =
    ( Loading
    , getPackages
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fail httpError ->
            ( Failed httpError
            , Cmd.none
            )

        Load packages ->
            ( Success
                { packages = packages
                , query = ""
                }
            , Cmd.none
            )

        Query query ->
            flip (,) Cmd.none
                <| case model of
                    Success facts ->
                        Success { facts | query = query }

                    Loading ->
                        model

                    Failed _ ->
                        model


getPackages : Cmd Msg
getPackages =
    let
        decodeSafe =
            [ Decode.map Just Package.decoder, Decode.succeed Nothing ]
                |> Decode.oneOf
                |> Decode.list
    in
        "/all-package-docs.json"
            |> Http.get decodeSafe
            |> Task.perform Fail
                (\maybePackages ->
                    Load (List.filterMap identity maybePackages)
                )
