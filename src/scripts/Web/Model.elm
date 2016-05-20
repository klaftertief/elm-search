module Web.Model exposing (..)

-- where

import Dict
import Http
import Package.Package as Package exposing (Package)
import Package.Version as Version exposing (Version)
import Search.Model as Search
import String


type Model
    = Loading String
    | Failed Http.Error
    | Ready Search.Model


type Msg
    = Fail Http.Error
    | Load (List Package)
    | Search Search.Msg
    | LocationSearchChange String


type alias Flags =
    { search : String }


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


parseSearchString : String -> Search.Filter
parseSearchString searchString =
    case String.uncons (Http.uriDecode searchString) of
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

                queryString =
                    Dict.get "q" parts |> Maybe.withDefault ""

                query =
                    Search.maybeQueryFromString queryString

                elmVersion =
                    Dict.get "v" parts
                        |> Maybe.withDefault ""
                        |> Search.maybeVersionFromString
            in
                { queryString = queryString
                , query = query
                , elmVersion = elmVersion
                }

        _ ->
            Search.initialFilter
