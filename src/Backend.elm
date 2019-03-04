port module Backend exposing (main)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Type
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
    { packages : Dict String Package }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { packages = Dict.empty }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotPackageJson Json.Decode.Value
    | GotSearchRequest String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackageJson packageJson ->
            case Json.Decode.decodeValue packageDecoder packageJson of
                Ok package ->
                    -- let
                    --     _ =
                    --         Debug.log "added" package.info.name
                    -- in
                    ( { model | packages = insertPackage package model.packages }
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "could not add package" (Json.Decode.errorToString err)
                    in
                    ( model, Cmd.none )

        GotSearchRequest queryString ->
            ( model
            , result
                (Json.Encode.object
                    [ ( "query", Json.Encode.string queryString )
                    , ( "result"
                      , model.packages
                            |> Dict.values
                            |> List.concatMap .modules
                            |> List.concatMap .values
                            |> List.filter (\value -> queryString == value.name)
                            |> Json.Encode.list
                                (\value ->
                                    Json.Encode.object
                                        [ ( "name", Json.Encode.string value.name )
                                        , ( "comment", Json.Encode.string value.comment )
                                        , ( "type", Json.Encode.string (elmTypeToString False value.tipe) )
                                        ]
                                )
                      )
                    ]
                )
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ addPackage GotPackageJson
        , search GotSearchRequest
        ]



-- PORTS


port addPackage : (Json.Decode.Value -> msg) -> Sub msg


port search : (String -> msg) -> Sub msg


port result : Json.Decode.Value -> Cmd msg



-- PACKAGE


type alias Package =
    { info : Elm.Project.PackageInfo
    , readme : String
    , modules : List Elm.Docs.Module
    }


insertPackage : Package -> Dict String Package -> Dict String Package
insertPackage package =
    Dict.insert (Elm.Package.toString package.info.name) package


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


elmTypeToString : Bool -> Elm.Type.Type -> String
elmTypeToString nested tipe =
    case tipe of
        Elm.Type.Var name ->
            name

        Elm.Type.Lambda ((Elm.Type.Lambda fromFrom fromTo) as fromLamda) to ->
            "(" ++ elmTypeToString False fromLamda ++ ") -> " ++ elmTypeToString False to

        Elm.Type.Lambda from to ->
            elmTypeToString False from ++ " -> " ++ elmTypeToString False to

        Elm.Type.Tuple [] ->
            "()"

        Elm.Type.Tuple types ->
            "( " ++ (types |> List.map (elmTypeToString False) |> String.join ", ") ++ " )"

        Elm.Type.Type name types ->
            name
                :: List.map (elmTypeToString True) types
                |> String.join " "
                |> (\typeString ->
                        if nested then
                            "(" ++ typeString ++ ")"

                        else
                            typeString
                   )

        Elm.Type.Record fields (Just extensible) ->
            "{ " ++ extensible ++ " | " ++ recordFieldsToString fields ++ " }"

        Elm.Type.Record fields Nothing ->
            "{ " ++ recordFieldsToString fields ++ " }"


recordFieldsToString : List ( String, Elm.Type.Type ) -> String
recordFieldsToString fields =
    fields
        |> List.map (\( name, tipe ) -> name ++ " : " ++ elmTypeToString False tipe)
        |> String.join ", "
