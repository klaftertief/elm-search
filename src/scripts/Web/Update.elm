module Web.Update exposing (..)

import Docs.Package as Package
import Json.Decode as Decode
import Ports
import Search.Model as Search
import Search.Update as Search
import Web.Model as Model exposing (..)


init : List Package.Package -> Flags -> ( Model, Cmd Msg )
init packages { index, search } =
    let
        searchModel =
            Search.init (parseSearchString search) packages
    in
    ( Ready searchModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
