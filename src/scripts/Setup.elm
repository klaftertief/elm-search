port module Setup exposing (main)

import Blacklist
import Docs.Package exposing (Entry, Module, Package)
import Generate
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Set
import Task


type alias Model =
    { packages : List Package
    , remaining : Int
    , elmVersion : String
    }


type Msg
    = All (List Package)
    | Local Package
    | Fetch Package
    | Response Decode.Value Package


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        elmVersion =
            "0.18.0"

        getNewPackageNames =
            Decode.list Decode.string
                |> Http.get "http://package.elm-lang.org/new-packages"
                |> Http.toTask

        getAllPackages =
            Decode.list Docs.Package.remoteMetaDataDecoder
                |> Http.get "http://package.elm-lang.org/all-packages"
                |> Http.toTask
    in
    ( Model [] -1 elmVersion
    , Task.map2 onlyNewPackages getNewPackageNames getAllPackages
        |> Task.attempt (All << ensureOk)
    )


onlyNewPackages : List String -> List Package -> List Package
onlyNewPackages newPackageNames =
    let
        new =
            Set.fromList newPackageNames
    in
    List.filter <|
        \{ user, name } ->
            Set.member (user ++ "/" ++ name) new


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        All packages ->
            let
                newModel =
                    { model | remaining = List.length packages - Blacklist.length }
            in
            ( newModel, Cmd.batch (List.map checkCache packages) )

        Local package ->
            let
                newModel =
                    withNew package model
            in
            ( newModel, attemptFinish newModel )

        Fetch package ->
            ( model, fetchModules model.elmVersion package )

        Response json package ->
            let
                newModel =
                    withNew package model

                cache =
                    outbox <|
                        Encode.object
                            [ ( "tag", Encode.string "CACHE" )
                            , ( "package", Docs.Package.simpleEncoder package json )
                            ]
            in
            ( newModel, Cmd.batch [ cache, attemptFinish newModel ] )


withNew : Package -> Model -> Model
withNew package model =
    { model
        | packages = package :: model.packages
        , remaining = model.remaining - 1
    }


checkCache : Package -> Cmd Msg
checkCache package =
    outbox <|
        Encode.object
            [ ( "tag", Encode.string "CHECK_CACHE" )
            , ( "package", Docs.Package.simpleEncoder package Docs.Package.empty )
            ]


attemptFinish : Model -> Cmd Msg
attemptFinish { remaining, packages } =
    if remaining == 0 then
        outbox <|
            Encode.object
                [ ( "tag", Encode.string "DONE" )
                , ( "code", Encode.string <| Generate.elm packages )
                ]
    else
        Cmd.none


fetchModules : String -> Package -> Cmd Msg
fetchModules elmVersion package =
    if Blacklist.contains package then
        Cmd.none
    else
        let
            url =
                String.join "/"
                    [ "http://package.elm-lang.org/packages"
                    , Docs.Package.identifier package
                    , "documentation.json"
                    ]

            decoder =
                Decode.map2 Response
                    Decode.value
                    (Docs.Package.withModulesDecoder elmVersion package)
        in
        Http.get url decoder
            |> Http.send ensureOk


ensureOk : Result x a -> a
ensureOk result =
    case result of
        Ok value ->
            value

        Err _ ->
            Debug.crash "fatal error"


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        decoder =
            Decode.andThen
                (actionDecoder model.elmVersion)
                (Decode.field "tag" Decode.string)
    in
    inbox <| ensureOk << Decode.decodeValue decoder


actionDecoder : String -> String -> Decode.Decoder Msg
actionDecoder elmVersion tag =
    case tag of
        "CACHE_HIT" ->
            Decode.map Local
                (Decode.field "package" (Docs.Package.simpleDecoder elmVersion))

        "CACHE_MISS" ->
            Decode.map Fetch
                (Decode.field "package" (Docs.Package.simpleDecoder elmVersion))

        _ ->
            Decode.fail "BAD INCOMING PORT TAG"


port inbox : (Decode.Value -> msg) -> Sub msg


port outbox : Encode.Value -> Cmd msg
