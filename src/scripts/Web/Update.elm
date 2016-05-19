module Web.Update exposing (..)

-- where

import Http
import Json.Decode as Decode
import Task
import Package.Module.Type as Type exposing (Type)
import Package.Package as Package
import Package.Version as Version
import Search.Chunk as Chunk
import Set exposing (Set)
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
            let
                chunks =
                    List.concatMap Chunk.packageChunks packages

                elmVersionsList =
                    chunks
                        |> List.map .elmVersion
                        |> List.filterMap identity
            in
                ( Success
                    { chunks = chunks
                    , filteredChunks = []
                    , query = ""
                    , queryType = Nothing
                    , elmVersions = Set.fromList elmVersionsList
                    , elmVersionsFilter = Nothing
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
            flip (,) Cmd.none
                <| case model of
                    Success facts ->
                        let
                            queryType =
                                Type.parse facts.query
                                    |> Result.toMaybe

                            filteredChunks =
                                search facts.elmVersionsFilter queryType facts.chunks
                        in
                            Success
                                { facts
                                    | queryType = queryType
                                    , filteredChunks = filteredChunks
                                }

                    _ ->
                        model

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
