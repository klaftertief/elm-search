module Web.Model exposing (..)

import Dict
import Http
import Docs.Package as Package exposing (Package)
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
    let
        pairs =
            if String.isEmpty queryString then
                []
            else
                [ ( "q", queryString ) ]
    in
        "?" ++ String.join "&" (List.map queryPair pairs)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    Http.uriEncode key ++ "=" ++ Http.uriEncode value


parseSearchString : String -> Search.Filter
parseSearchString searchString =
    case String.uncons searchString of
        Just ( '?', rest ) ->
            let
                parts =
                    String.split "&" rest
                        |> List.map (String.split "=")
                        |> List.filterMap
                            (\pair ->
                                case pair of
                                    [ k, v ] ->
                                        Just ( Http.uriDecode k, Http.uriDecode v )

                                    _ ->
                                        Nothing
                            )
                        |> Dict.fromList

                queryString =
                    Dict.get "q" parts |> Maybe.withDefault ""

                query =
                    Search.queryListFromString queryString
            in
                { queryString = queryString
                , query = query
                , lastQuery = ""
                }

        _ ->
            Search.initialFilter
