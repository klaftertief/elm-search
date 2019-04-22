port module Backend exposing (main)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Search as Search
import Elm.Search.Index as Index exposing (Index)
import Elm.Search.Query as Query
import Elm.Type
import Elm.Version
import Json.Decode
import Json.Encode
import Route
import Url


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { index : Index }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { index = Index.empty }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotPackageJson Json.Decode.Value
    | GotRequest String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackageJson packageJson ->
            case Json.Decode.decodeValue packageDecoder packageJson of
                Ok package ->
                    ( { model | index = Index.addPackage package model.index }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

        GotRequest url ->
            let
                maybeRoute =
                    Url.fromString url
                        |> Maybe.andThen Route.fromUrl
            in
            case maybeRoute of
                Just (Route.Search (Just queryString)) ->
                    ( model
                    , response
                        (Json.Encode.object
                            [ ( "url", Json.Encode.string url )
                            , ( "response"
                              , Search.search (Query.fromString queryString) model.index
                                    |> List.filter (Tuple.first >> (\d -> d < 0.2))
                                    |> List.sortBy Tuple.first
                                    |> List.take 20
                                    |> Json.Encode.list (Tuple.second >> Index.encodeBlock)
                              )
                            ]
                        )
                    )

                Just (Route.Search Nothing) ->
                    ( model
                    , response
                        (Json.Encode.object
                            [ ( "url", Json.Encode.string url )
                            , ( "response", Json.Encode.string "No query provided" )
                            ]
                        )
                    )

                Just Route.Home ->
                    ( model
                    , response
                        (Json.Encode.object
                            [ ( "url", Json.Encode.string url )
                            , ( "response", Json.Encode.string "elm-search home" )
                            ]
                        )
                    )

                Just Route.Packages ->
                    ( model
                    , response
                        (Json.Encode.object
                            [ ( "url", Json.Encode.string url )
                            , ( "response"
                              , Index.allPackages model.index
                                    |> List.map Index.Package
                                    |> Json.Encode.list Index.encodeBlock
                              )
                            ]
                        )
                    )

                Just (Route.Package id) ->
                    ( model
                    , response
                        (Json.Encode.object
                            [ ( "url", Json.Encode.string url )
                            , ( "response"
                              , Index.getPackage id model.index
                                    |> Maybe.map (Index.Package >> Index.encodeBlock)
                                    |> Maybe.withDefault Json.Encode.null
                              )
                            ]
                        )
                    )

                Nothing ->
                    ( model
                    , response
                        (Json.Encode.object
                            [ ( "url", Json.Encode.string url )
                            , ( "response", Json.Encode.string "Route not found" )
                            ]
                        )
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ addPackage GotPackageJson
        , request GotRequest
        ]



-- PORTS


port addPackage : (Json.Decode.Value -> msg) -> Sub msg


port request : (String -> msg) -> Sub msg


port response : Json.Decode.Value -> Cmd msg



-- PACKAGE


type alias Package =
    { info : Elm.Project.PackageInfo
    , readme : String
    , modules : List Elm.Docs.Module
    }


packageDecoder : Json.Decode.Decoder Package
packageDecoder =
    Json.Decode.map3 Package
        (Json.Decode.field "package" packageInfoDecoder)
        (Json.Decode.field "readme" Json.Decode.string)
        (Json.Decode.field "docs" docsDecoder)


packageInfoDecoder : Json.Decode.Decoder Elm.Project.PackageInfo
packageInfoDecoder =
    Json.Decode.andThen
        (\project ->
            case project of
                Elm.Project.Package packageInfo ->
                    Json.Decode.succeed packageInfo

                Elm.Project.Application _ ->
                    Json.Decode.fail "Applications are not supported"
        )
        Elm.Project.decoder


docsDecoder : Json.Decode.Decoder (List Elm.Docs.Module)
docsDecoder =
    Json.Decode.list Elm.Docs.decoder
