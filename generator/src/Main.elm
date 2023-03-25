port module Main exposing (main)

import Dict exposing (Dict)
import Docs.Search
import Elm.Docs
import Elm.Package
import Elm.Project exposing (Project)
import Elm.Version
import Http
import Json.Decode
import Task exposing (Task)


type Model
    = Model
        { packages : Packages
        }


type alias Packages =
    Dict ( String, String, String ) State


type State
    = NotAsked
    | Cached
    | Loading
    | LoadedRaw RawPackage
    | Loaded Package
    | Written
    | Failed


type alias RawPackage =
    { project : String
    , readme : String
    , docs : String
    }


type alias Package =
    { project : Elm.Project.Project
    , readme : String
    , docs : List Elm.Docs.Module
    }


type Msg
    = GotAllPackagesResponse (Result Http.Error (List Docs.Search.Entry))
    | GotCacheMiss ( String, String, String )
    | GotCacheHit ( String, String, String )
    | GotRawPackageResponse ( String, String, String ) (Result Http.Error RawPackage)
    | GotPackageResponse ( String, String, String ) (Result Http.Error Package)


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model { packages = Dict.empty }
    , Cmd.batch
        [ log "Generator"
        , getAllPackages
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        GotAllPackagesResponse (Err _) ->
            ( Model model
            , log "Got packages error"
            )

        GotAllPackagesResponse (Ok packages) ->
            let
                newModel =
                    { model
                        | packages =
                            packages
                                -- |> List.filter (\package -> package.author == "elm")
                                |> List.map
                                    (\package ->
                                        ( ( package.author
                                          , package.project
                                          , Elm.Version.toString package.version
                                          )
                                        , NotAsked
                                        )
                                    )
                                |> Dict.fromList
                    }
            in
            ( Model newModel
            , Cmd.batch
                [ log ("Got packages success: " ++ String.fromInt (List.length packages))
                , newModel.packages
                    |> Dict.keys
                    --|> List.map lookupCache
                    |> List.map
                        (\identifier ->
                            getRawPackage (GotRawPackageResponse identifier) identifier
                        )
                    |> Cmd.batch
                ]
            )

        GotCacheMiss (( user, project, version ) as identifier) ->
            let
                newModel =
                    Model
                        { model
                            | packages =
                                Dict.insert identifier
                                    Loading
                                    model.packages
                        }
            in
            ( newModel
            , Cmd.batch
                [ log ("Got cache miss: " ++ String.join "/" [ user, project, version ])
                , getPackage (GotPackageResponse identifier) identifier
                ]
            )

        GotCacheHit (( user, project, version ) as identifier) ->
            let
                newModel =
                    Model
                        { model
                            | packages =
                                Dict.insert identifier
                                    Cached
                                    model.packages
                        }
            in
            ( newModel
            , Cmd.batch
                [ log ("Got cache hit: " ++ String.join "/" [ user, project, version ])
                , maybeWriteIndex newModel
                ]
            )

        GotPackageResponse (( user, project, version ) as identifier) (Err e) ->
            let
                newModel =
                    Model
                        { model
                            | packages =
                                Dict.insert identifier
                                    Failed
                                    model.packages
                        }
            in
            ( newModel
            , Cmd.batch
                [ log ("Got package error: " ++ String.join "/" [ user, project, version ] ++ "\n" ++ Debug.toString e)
                , maybeWriteIndex newModel
                ]
            )

        GotPackageResponse (( user, project, version ) as identifier) (Ok package) ->
            let
                newModel =
                    Model
                        { model
                            | packages =
                                Dict.insert identifier
                                    (Loaded package)
                                    model.packages
                        }
            in
            ( newModel
            , Cmd.batch
                [ log ("Got package success: " ++ String.join "/" [ user, project, version ])
                , writeCache { identifier = identifier, code = packageToCode identifier package }
                , maybeWriteIndex newModel
                ]
            )

        GotRawPackageResponse (( user, project, version ) as identifier) (Err e) ->
            let
                newModel =
                    Model
                        { model
                            | packages =
                                Dict.insert identifier
                                    Failed
                                    model.packages
                        }
            in
            ( newModel
            , Cmd.batch
                [ log ("Got raw package error: " ++ String.join "/" [ user, project, version ] ++ "\n" ++ Debug.toString e)
                ]
            )

        GotRawPackageResponse (( user, project, version ) as identifier) (Ok rawPackage) ->
            let
                newModel =
                    Model
                        { model
                            | packages =
                                Dict.insert identifier
                                    (LoadedRaw rawPackage)
                                    model.packages
                        }
            in
            ( newModel
            , Cmd.batch
                [ log ("Got raw package success: " ++ String.join "/" [ user, project, version ])
                , writeRawCache
                    { identifier = identifier
                    , rawPackage = rawPackage
                    }
                ]
            )


maybeWriteIndex : Model -> Cmd msg
maybeWriteIndex (Model model) =
    let
        undecided =
            Dict.filter
                (\_ state -> state == Loading || state == NotAsked)
                model.packages

        indexPackages =
            model.packages
                |> Dict.filter
                    (\_ state ->
                        case state of
                            Loaded _ ->
                                True

                            Written ->
                                True

                            Cached ->
                                True

                            NotAsked ->
                                False

                            Loading ->
                                False

                            Failed ->
                                False

                            LoadedRaw _ ->
                                False
                    )
                |> Dict.keys
    in
    if Dict.isEmpty undecided then
        Cmd.batch
            [ writeIndex
                { code =
                    packagesToIndexCode indexPackages
                }
            , log ("Writing index: " ++ String.fromInt (List.length indexPackages))
            ]

    else
        Cmd.none


packagesToIndexCode : List ( String, String, String ) -> String
packagesToIndexCode identifiers =
    [ "module Index exposing (index)"
    , "\n"
    , identifiers
        |> List.map
            (\( user, project, version ) ->
                "import "
                    ++ String.join "__" [ "Package", user, project, String.replace "." "_" version ]
                    |> String.replace "-" "_"
            )
        |> String.join "\n"
    , "\n"
    , "index = "
    , "    [ "
        ++ (identifiers
                |> List.map
                    (\( user, project, version ) ->
                        String.join "__" [ "Package", user, project, String.replace "." "_" version ]
                            ++ ".package"
                            |> String.replace "-" "_"
                    )
                |> String.join "\n    , "
           )
    , "    ]"
    ]
        |> String.join "\n"


packageToCode : ( String, String, String ) -> Package -> String
packageToCode ( user, project, version ) package =
    [ "module " ++ String.join "__" [ "Package", user, project, String.replace "." "_" version ] ++ " exposing (package)" |> String.replace "-" "_"
    , "\n"
    , "import Elm.Docs exposing (Associativity(..))"
    , "import Elm.Type exposing (Type(..))"
    , "\n"
    , "package = "
    , "    { docs = "
    , "        [ "
    , "          " ++ (package.docs |> List.map moduleToCode |> String.join "\n        , ")
    , "        ] "
    , "    , readme = "
        ++ Debug.toString package.readme
        |> String.replace "], comment = \"" "]\n    , comment = \""
    , "    , project = "
    , "        { user = " ++ Debug.toString user
    , "        , name = " ++ Debug.toString project
    , "        , version = " ++ Debug.toString version
    , "        , summary = " ++ Debug.toString (packageProjectSummary package.project)
    , "        }"
    , "    }"
    ]
        |> String.join "\n"
        |> String.replace "</script" "</\" ++ \"script"


moduleToCode : Elm.Docs.Module -> String
moduleToCode module_ =
    [ "        { name = " ++ Debug.toString module_.name
    , "        , comment = " ++ Debug.toString module_.comment
    , "        , aliases = "
    , "            [ "
    , "              " ++ (module_.aliases |> List.map Debug.toString |> String.join "\n            , ")
    , "            ] "
    , "        , binops = " ++ Debug.toString module_.binops
    , "        , unions = "
    , "            [ "
    , "              " ++ (module_.unions |> List.map Debug.toString |> String.join "\n            , ")
    , "            ] "
    , "        , values = "
    , "            [ "
    , "              " ++ (module_.values |> List.map Debug.toString |> String.join "\n            , ")
    , "            ] "
    , "        }"
    ]
        |> String.join "\n"


packageProjectSummary : Project -> String
packageProjectSummary project =
    case project of
        Elm.Project.Application _ ->
            ""

        Elm.Project.Package packageInfo ->
            packageInfo.summary


getAllPackages : Cmd Msg
getAllPackages =
    Http.get
        { url = "https://package.elm-lang.org/search.json"
        , expect =
            Http.expectJson GotAllPackagesResponse
                (Json.Decode.list Docs.Search.decoder)
        }


getPackage : (Result Http.Error Package -> msg) -> ( String, String, String ) -> Cmd msg
getPackage toMsg =
    getPackageTask >> Task.attempt toMsg


getRawPackage : (Result Http.Error RawPackage -> msg) -> ( String, String, String ) -> Cmd msg
getRawPackage toMsg =
    getRawPackageTask >> Task.attempt toMsg


getRawPackageTask : ( String, String, String ) -> Task Http.Error RawPackage
getRawPackageTask identifier =
    Task.map3 RawPackage
        (getRawPackageProjectTask identifier)
        (getPackageReadmeTask identifier)
        (getRawPackageDocsTask identifier)


getPackageTask : ( String, String, String ) -> Task Http.Error Package
getPackageTask identifier =
    Task.map3 Package
        (getPackageProjectTask identifier)
        (getPackageReadmeTask identifier)
        (getPackageDocsTask identifier)


getPackageProjectTask : ( String, String, String ) -> Task Http.Error Elm.Project.Project
getPackageProjectTask ( author, project, version ) =
    let
        url =
            String.join "/"
                [ "https://package.elm-lang.org/packages"
                , author
                , project
                , version
                , "elm.json"
                ]
    in
    getJsonTask Elm.Project.decoder url


getRawPackageProjectTask : ( String, String, String ) -> Task Http.Error String
getRawPackageProjectTask ( author, project, version ) =
    let
        url =
            String.join "/"
                [ "https://package.elm-lang.org/packages"
                , author
                , project
                , version
                , "elm.json"
                ]
    in
    getStringTask url


getPackageReadmeTask : ( String, String, String ) -> Task Http.Error String
getPackageReadmeTask ( author, project, version ) =
    let
        url =
            String.join "/"
                [ "https://package.elm-lang.org/packages"
                , author
                , project
                , version
                , "README.md"
                ]
    in
    getStringTask url


getPackageDocsTask : ( String, String, String ) -> Task Http.Error (List Elm.Docs.Module)
getPackageDocsTask ( author, project, version ) =
    let
        url =
            String.join "/"
                [ "https://package.elm-lang.org/packages"
                , author
                , project
                , version
                , "docs.json"
                ]
    in
    getJsonTask (Json.Decode.list Elm.Docs.decoder) url


getRawPackageDocsTask : ( String, String, String ) -> Task Http.Error String
getRawPackageDocsTask ( author, project, version ) =
    let
        url =
            String.join "/"
                [ "https://package.elm-lang.org/packages"
                , author
                , project
                , version
                , "docs.json"
                ]
    in
    getStringTask url


getJsonTask : Json.Decode.Decoder value -> String -> Task Http.Error value
getJsonTask decoder =
    getTask
        (\body ->
            case Json.Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Json.Decode.errorToString err))
        )


getStringTask : String -> Task Http.Error String
getStringTask =
    getTask Ok


getTask : (String -> Result Http.Error value) -> String -> Task Http.Error value
getTask resolveBody url =
    Http.task
        { method = "GET"
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , url = url
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Err (Http.BadUrl url_)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ _ body ->
                            resolveBody body
                )
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ cacheMiss GotCacheMiss
        , cacheHit GotCacheHit
        ]


port log : String -> Cmd msg


port lookupCache : ( String, String, String ) -> Cmd msg


port writeCache :
    { identifier : ( String, String, String )
    , code : String
    }
    -> Cmd msg


port writeRawCache :
    { identifier : ( String, String, String )
    , rawPackage : RawPackage
    }
    -> Cmd msg


port writeIndex :
    { code : String
    }
    -> Cmd msg


port cacheMiss : (( String, String, String ) -> msg) -> Sub msg


port cacheHit : (( String, String, String ) -> msg) -> Sub msg
