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
    | GotSearchRequest String
    | GotAllPackagesRequest
    | GotSinglePackageRequest ( String, String, String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackageJson packageJson ->
            case Json.Decode.decodeValue packageDecoder packageJson of
                Ok package ->
                    let
                        _ =
                            Debug.log "Added package" package.info.name
                    in
                    ( { model | index = Index.addPackage package model.index }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

        GotSearchRequest queryString ->
            ( model
            , result
                (Json.Encode.object
                    [ ( "query", Json.Encode.string queryString )
                    , ( "result"
                      , Search.search (Query.fromString queryString |> Debug.log "q") model.index
                            |> List.filter (Tuple.first >> (\d -> d < 0.2))
                            |> List.sortBy Tuple.first
                            |> List.take 20
                            |> Json.Encode.list (Tuple.second >> Index.encodeBlock)
                      )
                    ]
                )
            )

        GotAllPackagesRequest ->
            ( model
            , result
                (Json.Encode.object
                    [ ( "query", Json.Encode.string "_packages" )
                    , ( "result", Json.Encode.null )
                    ]
                )
            )

        GotSinglePackageRequest ( user, name, version ) ->
            ( model
            , result
                (Json.Encode.object
                    [ ( "query", Json.Encode.string (String.join "_" [ "_packages", user, name, version ]) )
                    , ( "result", Json.Encode.null )
                    ]
                )
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ addPackage GotPackageJson
        , search GotSearchRequest
        , listPackages (\_ -> GotAllPackagesRequest)
        , getPackage GotSinglePackageRequest
        ]



-- PORTS


port addPackage : (Json.Decode.Value -> msg) -> Sub msg


port search : (String -> msg) -> Sub msg


port listPackages : (() -> msg) -> Sub msg


port getPackage : (( String, String, String ) -> msg) -> Sub msg


port result : Json.Decode.Value -> Cmd msg



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
