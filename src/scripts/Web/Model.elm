module Web.Model exposing (..)

import Dict
import Docs.Package as Package exposing (Package)
import Http
import Search.Model as Search
import String


type Model
    = Loading Search.Filter
    | Failed Http.Error
    | Ready Search.Model


type Msg
    = Fail Http.Error
    | Load (List Package)
    | SearchMsg Search.Msg
    | LocationSearchChange String


type alias Flags =
    { index : String
    , search : String
    }


toQueryString : Search.Filter -> String
toQueryString { queryString } =
    if String.isEmpty queryString then
        ""
    else
        "?q=" ++ Http.encodeUri queryString


decodeQuery : String -> String
decodeQuery query =
    String.join "%20" (String.split "+" query)


parseSearchString : String -> Search.Filter
parseSearchString searchString =
    case String.uncons (decodeQuery searchString) of
        Just ( '?', rest ) ->
            let
                parts =
                    String.split "&" rest
                        |> List.map (String.split "=")
                        |> List.filterMap
                            (\pair ->
                                case pair of
                                    [ k, v ] ->
                                        Maybe.map2 (,)
                                            (Http.decodeUri k)
                                            (Http.decodeUri v)

                                    _ ->
                                        Nothing
                            )
                        |> Dict.fromList

                queryString =
                    Dict.get "q" parts
                        |> Maybe.withDefault ""

                query =
                    Search.queryListFromString queryString
            in
            { queryString = queryString
            , query = query
            , lastQuery = ""
            }

        _ ->
            Search.initialFilter
