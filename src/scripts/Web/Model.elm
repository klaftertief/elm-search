module Web.Model exposing (..)

-- where

import Http
import Package.Module.Type as Type exposing (Type)
import Package.Package as Package exposing (Package)
import Package.Version as Version exposing (Version)
import Search.Chunk as Chunk exposing (Chunk)
import Search.Distance as Distance
import Set exposing (Set)


type Model
    = Loading
    | Failed Http.Error
    | Success Info


type alias Info =
    { chunks : List Chunk
    , filteredChunks : List Chunk
    , query : String
    , queryType : Maybe Type
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


search : Maybe Version -> Maybe Type -> List Chunk -> List Chunk
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
                            ( distance - Distance.lowPenalty, chunk )
                        else if chunk.name.userName == "elm-lang" then
                            ( distance - Distance.lowPenalty / 2, chunk )
                        else if chunk.name.userName == "elm-community" then
                            ( distance - Distance.lowPenalty / 4, chunk )
                        else
                            ( distance, chunk )
                    )
                |> List.sortBy fst
                |> List.map snd
    in
        filteredChunks
