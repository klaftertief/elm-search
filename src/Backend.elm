port module Backend exposing (main)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Package
import Elm.Project
import Json.Decode


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { packages : Dict String Package }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { packages = Dict.empty }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotPackageJson Json.Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackageJson packageJson ->
            case Json.Decode.decodeValue packageDecoder packageJson of
                Ok package ->
                    ( { model | packages = insertPackage package model.packages }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    addPackage GotPackageJson



-- PORTS


port addPackage : (Json.Decode.Value -> msg) -> Sub msg



-- PACKAGE


type alias Package =
    { info : Elm.Project.PackageInfo
    , modules : List Elm.Docs.Module
    }


insertPackage : Package -> Dict String Package -> Dict String Package
insertPackage package =
    Dict.insert (Elm.Package.toString package.info.name) package


packageDecoder : Json.Decode.Decoder Package
packageDecoder =
    Json.Decode.map2 Package
        (Json.Decode.field "package" packageInfoDecoder)
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
