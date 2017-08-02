module Web.Update exposing (..)

import Docs.Package as Package
import Http
import Json.Decode as Decode
import Ports
import Search.Model as Search
import Search.Update as Search
import Task
import Web.Model as Model exposing (..)


init : Flags -> ( Model, Cmd Msg )
init { index, search } =
    let
        filter =
            parseSearchString search
    in
    ( Loading filter
    , getPackages index
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
                                    Ports.pushQuery (toQueryString search.filter)

                                _ ->
                                    Cmd.none
                    in
                    ( Ready newSearch, cmd )

                Loading filter ->
                    case searchMsg of
                        Search.SetFilterQueryString queryString ->
                            ( Loading
                                { filter
                                    | queryString = queryString
                                    , query = Search.queryListFromString queryString
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( Loading filter, Cmd.none )

                Failed _ ->
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


getPackages : String -> Cmd Msg
getPackages url =
    let
        decodeSafe =
            [ Decode.map Just Package.decoder, Decode.succeed Nothing ]
                |> Decode.oneOf
                |> Decode.list

        toMsg result =
            case result of
                Err e ->
                    Fail e

                Ok maybePackages ->
                    Load (List.filterMap identity maybePackages)
    in
    Http.get url decodeSafe
        |> Http.send toMsg
