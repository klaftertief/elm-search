module Web.Update exposing (..)

-- where

import Http
import Json.Decode as Decode
import Task
import Package.Module.Type as Type exposing (Type)
import Package.Package as Package
import Package.Version as Version
import Ports
import Search.Chunk as Chunk
import Set exposing (Set)
import Web.Model as Model exposing (..)


init : Flags -> ( Model, Cmd Msg )
init { search } =
    ( Loading search
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
                ( query, maybeVersion ) =
                    case model of
                        Loading queryString ->
                            parseQueryString queryString

                        _ ->
                            ( "", Nothing )

                chunks =
                    List.concatMap Chunk.packageChunks packages

                elmVersionsList =
                    chunks
                        |> List.map .elmVersion
                        |> List.filterMap identity
            in
                ( Success
                    <| handleSearch
                        { chunks = chunks
                        , filteredChunks = []
                        , query = query
                        , queryType = Nothing
                        , elmVersions = Set.fromList elmVersionsList
                        , elmVersionsFilter = maybeVersion
                        }
                , Cmd.none
                )

        SetQuery query ->
            flip (,) Cmd.none
                <| case model of
                    Success facts ->
                        Success
                            { facts
                                | query = query
                                , queryType = Nothing
                            }

                    _ ->
                        model

        SetVersionFilter vsnString ->
            flip (,) Cmd.none
                <| case model of
                    Success facts ->
                        let
                            maybeElmVersion =
                                vsnString
                                    |> Version.fromString
                                    |> Result.toMaybe
                        in
                            Success
                                { facts
                                    | elmVersionsFilter = maybeElmVersion
                                }

                    _ ->
                        model

        SearchQuery ->
            case model of
                Success info ->
                    let
                        newInfo =
                            handleSearch info
                    in
                        ( Success newInfo
                        , Ports.pushQuery (toQueryString newInfo.elmVersionsFilter newInfo.query)
                        )

                _ ->
                    ( model, Cmd.none )

        ResetQuery ->
            flip (,) Cmd.none
                <| case model of
                    Success facts ->
                        Success
                            { facts
                                | query = ""
                                , queryType = Nothing
                                , filteredChunks = []
                            }

                    _ ->
                        model

        LocationSearchChange queryString ->
            let
                ( query, maybeVersion ) =
                    parseQueryString queryString

                newModel =
                    case model of
                        Success info ->
                            Success
                                (handleSearch
                                    { info
                                        | query = query
                                        , elmVersionsFilter = maybeVersion
                                    }
                                )

                        _ ->
                            model
            in
                ( newModel, Cmd.none )


handleSearch : Info -> Info
handleSearch info =
    let
        queryType =
            Type.parse info.query
                |> Result.toMaybe

        filteredChunks =
            search info.elmVersionsFilter queryType info.chunks
    in
        { info
            | queryType = queryType
            , filteredChunks = filteredChunks
        }


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
