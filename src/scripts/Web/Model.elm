module Web.Model exposing (..)

-- where

import Dict
import Http
import Package.Module.Type as Type
import Package.Package as Package exposing (Package)
import Package.Version as Version exposing (Version)
import Search.Chunk as Chunk exposing (Chunk)
import Search.Distance as Distance
import Set exposing (Set)
import String


type Model
    = Loading String
    | Failed Http.Error
    | Success Info


type alias Info =
    { chunks : List Chunk
    , filteredChunks : List Chunk
    , query : String
    , queryType : Maybe Type.Type
    , elmVersions : Set Version
    , elmVersionsFilter : Maybe Version
    }


type Msg
    = Fail Http.Error
    | Load (List Package)
    | SetQuery String
    | SetVersionFilter String
    | SearchQuery
    | ResetQuery
    | LocationSearchChange String


type alias Flags =
    { search : String }


search : Maybe Version -> Maybe Type.Type -> List Chunk -> List Chunk
search maybeVersionsFilter maybeQueryType chunks =
    let
        versionChunks =
            case maybeVersionsFilter of
                Just vsn ->
                    chunks
                        |> List.filter (.elmVersion >> (==) maybeVersionsFilter)

                Nothing ->
                    chunks

        weightedChunks =
            case maybeQueryType of
                Just tipe ->
                    case tipe of
                        Type.Var str ->
                            versionChunks
                                |> List.map (\chunk -> ( Distance.name str chunk, chunk ))

                        _ ->
                            versionChunks
                                |> List.map (\chunk -> ( Distance.tipe tipe chunk, chunk ))

                Nothing ->
                    []

        filteredChunks =
            weightedChunks
                |> List.filter (\( distance, _ ) -> distance <= Distance.lowPenalty)
                |> List.map
                    (\( distance, chunk ) ->
                        if chunk.name.userName == "elm-lang" && chunk.name.packageName == "core" then
                            ( distance - Distance.lowPenalty / 2, chunk )
                        else if chunk.name.userName == "elm-lang" then
                            ( distance - Distance.lowPenalty / 3, chunk )
                        else if chunk.name.userName == "elm-community" then
                            ( distance - Distance.lowPenalty / 4, chunk )
                        else
                            ( distance, chunk )
                    )
                |> List.sortBy fst
                |> List.map snd
    in
        filteredChunks


toQueryString : Maybe Version -> String -> String
toQueryString maybeVersionsFilter query =
    let
        start =
            if String.isEmpty query then
                []
            else
                [ ( "q", query ) ]

        queries =
            case maybeVersionsFilter of
                Just vsn ->
                    start ++ [ ( "v", Version.vsnToString vsn ) ]

                Nothing ->
                    start
    in
        if List.isEmpty queries then
            ""
        else
            "?" ++ String.join "&" (List.map queryPair queries)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    Http.uriEncode key ++ "=" ++ Http.uriEncode value


parseQueryString : String -> ( String, Maybe Version )
parseQueryString queryString =
    case String.uncons (Http.uriDecode queryString) of
        Just ( '?', rest ) ->
            let
                parts =
                    String.split "&" rest
                        |> List.map (String.split "=")
                        |> List.filterMap
                            (\pair ->
                                case pair of
                                    [ k, v ] ->
                                        Just ( k, v )

                                    _ ->
                                        Nothing
                            )
                        |> Dict.fromList
            in
                ( Dict.get "q" parts |> Maybe.withDefault ""
                , Dict.get "v" parts
                    |> Maybe.map Version.fromString
                    |> Maybe.map Result.toMaybe
                    |> Maybe.withDefault Nothing
                )

        _ ->
            ( "", Nothing )
