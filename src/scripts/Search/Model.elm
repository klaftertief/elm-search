module Search.Model exposing (..)

import Docs.Package as Package exposing (Package)
import Docs.Type as Type
import Search.Chunk as Chunk exposing (Chunk)
import Search.Distance as Distance
import String


type alias Model =
    { index : Index
    , filter : Filter
    , result : Result
    }


type alias Index =
    { chunks : List Chunk
    }


type alias Filter =
    { queryString : String
    , query : List Query
    , lastQuery : String
    }


type Query
    = Name String
    | Type Type.Type
    | User String
    | Package String
    | Module String


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
    }


initialFilter : Filter
initialFilter =
    { queryString = ""
    , query = []
    , lastQuery = ""
    }


initialResult : Result
initialResult =
    { chunks = [] }


type Msg
    = BuildIndex (List Package)
    | SetFilter Filter
    | SetFilterQueryString String
    | SetFilterQueryStringAndRunFilter String
    | RunFilter


queryListFromString : String -> List Query
queryListFromString string =
    if String.isEmpty string then
        []
    else
        [ if String.startsWith "user:" string then
            User (String.dropLeft 5 string)
          else if String.startsWith "package:" string then
            Package (String.dropLeft 8 string)
          else if String.startsWith "module:" string then
            Module (String.dropLeft 7 string)
          else
            case Type.parse string of
                Ok tipe ->
                    case tipe of
                        Type.Var _ ->
                            Name string

                        _ ->
                            Type tipe

                Err _ ->
                    Name string
        ]


buildIndex : List Package -> Index
buildIndex packages =
    { chunks = List.concatMap Chunk.packageChunks packages }


runFilter : Filter -> Index -> Result
runFilter { query } { chunks } =
    let
        resultChunks =
            case query of
                [] ->
                    []

                _ ->
                    List.foldl distanceByQuery (List.map (\c -> ( 0, c )) chunks) query
                        |> List.map (\( d, c ) -> ( d / toFloat (List.length query), c ))
                        |> filterByDistance (Distance.lowPenalty / 2)
                        |> prioritizeChunks
                        |> List.sortBy (\( d, c ) -> ( d, c.context.name, c.context.moduleName, c.context.packageName ))
                        |> List.map Tuple.second
    in
    { chunks = resultChunks }


distanceByQuery : Query -> List ( Float, Chunk ) -> List ( Float, Chunk )
distanceByQuery query chunks =
    let
        distance =
            case query of
                Name name ->
                    Distance.simple (.context >> .name) name

                Type tipe ->
                    Distance.tipe tipe

                User name ->
                    Distance.simple (.context >> .userName) name

                Package name ->
                    Distance.simple (.context >> .packageName) name

                Module name ->
                    Distance.simple (.context >> .moduleName) name
    in
    List.map (\( d, c ) -> ( d + distance c, c )) chunks


filterByDistance : Float -> List ( Float, Chunk ) -> List ( Float, Chunk )
filterByDistance distance weightedChunks =
    List.filter (Tuple.first >> (>=) distance) weightedChunks


prioritizeChunks : List ( Float, Chunk ) -> List ( Float, Chunk )
prioritizeChunks weightedChunks =
    List.map prioritizeChunk weightedChunks


prioritizeChunk : ( Float, Chunk ) -> ( Float, Chunk )
prioritizeChunk ( distance, chunk ) =
    let
        ( userName, packageName ) =
            ( chunk.context.userName, chunk.context.packageName )

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
