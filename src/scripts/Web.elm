port module Web exposing (program)

import Browser
import Dict
import Docs.Package exposing (Package)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Search.Model as Search
import Search.Update as Search
import Search.View as Search
import Url


program : List Package -> Program Flags Model Msg
program packages =
    Browser.element
        { init = init packages
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Ready Search.Model


type Msg
    = SearchMsg Search.Msg
    | LocationSearchChange String


type alias Flags =
    { search : String
    }


toQueryString : Search.Filter -> String
toQueryString { queryString } =
    if String.isEmpty queryString then
        ""

    else
        "?q=" ++ Url.percentEncode queryString


decodeQuery : String -> String
decodeQuery query_ =
    String.join "%20" (String.split "+" query_)


parseSearchString : String -> Search.Filter
parseSearchString searchString =
    case String.uncons (decodeQuery searchString) of
        Just ( '?', rest ) ->
            let
                parts =
                    String.split "&" rest
                        |> List.map (String.split "=")
                        |> List.filterMap decodePair
                        |> Dict.fromList

                queryString =
                    Dict.get "q" parts
                        |> Maybe.withDefault ""

                query_ =
                    Search.queryListFromString queryString
            in
            { queryString = queryString
            , query = query_
            , lastQuery = ""
            }

        _ ->
            Search.initialFilter


decodePair : List String -> Maybe ( String, String )
decodePair pair =
    case pair of
        [ k, v ] ->
            Maybe.map2 (\a b -> ( a, b )) (Url.percentDecode k) (Url.percentDecode v)

        _ ->
            Nothing


init : List Package -> Flags -> ( Model, Cmd Msg )
init packages { search } =
    let
        searchModel =
            Search.init (parseSearchString search) packages
    in
    ( Ready searchModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Ready search) =
    case msg of
        SearchMsg Search.RunFilter ->
            let
                newSearch =
                    Search.update Search.RunFilter search
            in
            ( Ready newSearch, toQueryString search.filter |> pushQuery )

        SearchMsg searchMsg ->
            let
                newSearch =
                    Search.update searchMsg search
            in
            ( Ready newSearch, Cmd.none )

        LocationSearchChange queryString ->
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


view : Model -> Html Msg
view (Ready search) =
    Html.map SearchMsg <|
        div [ class "searchReady" ]
            [ Search.viewSearchHeader search
            , Search.viewSearchBody search
            ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    query LocationSearchChange


port query : (String -> msg) -> Sub msg


port pushQuery : String -> Cmd msg
