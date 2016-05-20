module Web.Update exposing (..)

import Http
import Json.Decode as Decode
import Task
import Package.Package as Package
import Ports
import Search.Model as Search
import Search.Update as Search
import Web.Model as Model exposing (..)


init : Flags -> ( Model, Cmd Msg )
init { search } =
    let
        filter =
            parseSearchString search
    in
        ( Loading filter
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
            let
                filter =
                    case model of
                        Loading f ->
                            f

                        _ ->
                            Search.initialFilter

                search =
                    Search.init filter packages
            in
                ( Ready search
                , Cmd.none
                )

        SearchMsg searchMsg ->
            case model of
                Ready search ->
                    let
                        newSearch =
                            Search.update searchMsg search

                        cmd =
                            case searchMsg of
                                Search.RunFilter ->
                                    Ports.pushQuery (toQueryString search.filter.elmVersion search.filter.queryString)

                                _ ->
                                    Cmd.none
                    in
                        ( Ready newSearch, cmd )

                _ ->
                    ( model, Cmd.none )

        LocationSearchChange queryString ->
            case model of
                Ready search ->
                    let
                        filter =
                            parseSearchString queryString

                        newSearch =
                            if filter /= search.filter then
                                search
                                    |> Search.update (Search.SetFilter filter)
                                    |> Search.update Search.RunFilter
                            else
                                search
                    in
                        ( Ready newSearch, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
