module Search.Model exposing (..)

import Package.Module.Type as Type
import Package.Package as Package exposing (Package)
import Package.Version as Version exposing (Version)
import Search.Chunk as Chunk exposing (Chunk)
import Search.Distance as Distance
import Set exposing (Set)
import String


type alias Model =
    { index : Index
    , filter : Filter
    , result : Result
    }


type alias Index =
    { chunks : List Chunk
    , elmVersions : Set Version
    }


type alias Filter =
    { queryString : String
    , query : Maybe Query
    , elmVersion : Maybe Version
    }


type Query
    = Name String
    | Type Type.Type


type alias Result =
    { chunks : List Chunk }


initialModel : Model
initialModel =
    { index = initialIndex
    , filter = initialFilter
    , result = initialResult
    }


initialIndex : Index
initialIndex =
    { chunks = []
    , elmVersions = Set.empty
    }


initialFilter : Filter
initialFilter =
    { queryString = ""
    , query = Nothing
    , elmVersion = Nothing
    }


initialResult : Result
initialResult =
    { chunks = [] }


type Msg
    = BuildIndex (List Package)
    | SetFilter Filter
    | SetFilterQueryFrom String
    | SetFilterVersionFrom String
    | RunFilter


maybeQueryFromString : String -> Maybe Query
maybeQueryFromString string =
    if String.isEmpty string then
        Nothing
    else
        Just
            <| case Type.parse string of
                Ok tipe ->
                    case tipe of
                        Type.Var _ ->
                            Name string

                        _ ->
                            Type tipe

                Err _ ->
                    Name string


maybeVersionFromString : String -> Maybe Version
maybeVersionFromString string =
    string
        |> Version.fromString
        |> Result.toMaybe


buildIndex : List Package -> Index
buildIndex packages =
    let
        chunks =
            List.concatMap Chunk.packageChunks packages
                |> List.filter (.elmVersion >> (/=) Nothing)

        elmVersions =
            chunks
                |> List.map .elmVersion
                |> List.filterMap identity
                |> Set.fromList
    in
        { chunks = chunks
        , elmVersions = elmVersions
        }


runFilter : Filter -> Index -> Result
runFilter { query, elmVersion } { chunks } =
    let
        resultChunks =
            case query of
                Just filterQuery ->
                    chunks
                        |> filterByMaybeVersion elmVersion
                        |> distanceByQuery filterQuery
                        |> filterByDistance Distance.lowPenalty
                        |> prioritizeChunks
                        |> List.sortBy fst
                        |> List.map snd

                Nothing ->
                    []
    in
        { chunks = resultChunks }


filterByMaybeVersion : Maybe Version -> List Chunk -> List Chunk
filterByMaybeVersion maybeVersion chunks =
    case maybeVersion of
        Just version ->
            filterByVersion version chunks

        Nothing ->
            chunks


filterByVersion : Version -> List Chunk -> List Chunk
filterByVersion version chunks =
    List.filter (.elmVersion >> (==) (Just version)) chunks


distanceByQuery : Query -> List Chunk -> List ( Float, Chunk )
distanceByQuery query chunks =
    let
        distance =
            indexedPair
                <| case query of
                    Name name ->
                        Distance.name name

                    Type tipe ->
                        Distance.tipe tipe
    in
        List.map distance chunks


filterByDistance : Float -> List ( Float, Chunk ) -> List ( Float, Chunk )
filterByDistance distance weightedChunks =
    List.filter (fst >> (>=) distance) weightedChunks


prioritizeChunks : List ( Float, Chunk ) -> List ( Float, Chunk )
prioritizeChunks weightedChunks =
    List.map prioritizeChunk weightedChunks


prioritizeChunk : ( Float, Chunk ) -> ( Float, Chunk )
prioritizeChunk ( distance, chunk ) =
    let
        ( userName, packageName ) =
            ( chunk.name.userName, chunk.name.packageName )

        priority =
            Distance.lowPenalty
    in
        if userName == "elm-lang" && packageName == "core" then
            ( distance - priority / 2, chunk )
        else if userName == "elm-lang" then
            ( distance - priority / 3, chunk )
        else if userName == "elm-community" then
            ( distance - priority / 4, chunk )
        else
            ( distance, chunk )


indexedPair : (a -> b) -> a -> ( b, a )
indexedPair f x =
    ( f x, x )
