port module Download exposing (main)

import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version
import Http
import Json.Decode
import Json.Encode


type alias PackageMeta =
    { name : Elm.Package.Name
    , version : Elm.Version.Version
    }


packageMetaDecoder : Json.Decode.Decoder PackageMeta
packageMetaDecoder =
    Json.Decode.map2 PackageMeta
        (Json.Decode.field "name" Elm.Package.decoder)
        (Json.Decode.field "versions"
            (Json.Decode.list Elm.Version.decoder
                |> Json.Decode.andThen latestVersionDecoder
            )
        )


latestVersionDecoder : List Elm.Version.Version -> Json.Decode.Decoder Elm.Version.Version
latestVersionDecoder versions =
    case latestVersion versions of
        Just version ->
            Json.Decode.succeed version

        Nothing ->
            Json.Decode.fail "No published version"


latestVersion : List Elm.Version.Version -> Maybe Elm.Version.Version
latestVersion versions =
    versions |> List.reverse |> List.head


type alias Model =
    { packages : List PackageMeta }


type Msg
    = GotSearch (Result Http.Error (List PackageMeta))
    | GotInfo PackageMeta (Result Http.Error Json.Decode.Value)
    | GotReadme PackageMeta (Result Http.Error String)
    | GotModules PackageMeta (Result Http.Error Json.Decode.Value)


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { packages = [] }, searchAllPackages )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSearch (Ok packages) ->
            ( { model | packages = packages }, writeSearch packages ) |> downloadNext

        GotSearch (Err err) ->
            ( model, Cmd.none )

        GotInfo meta (Ok info) ->
            ( model, writeInfo meta info )

        GotInfo meta (Err err) ->
            let
                _ =
                    Debug.log "could not download info" meta
            in
            ( model, Cmd.none )

        GotReadme meta (Ok readme) ->
            ( model, writeReadme meta readme ) |> downloadNext

        GotReadme meta (Err err) ->
            let
                _ =
                    Debug.log "could not download readme" meta
            in
            ( model, Cmd.none ) |> downloadNext

        GotModules meta (Ok modules) ->
            ( model, writeModules meta modules )

        GotModules meta (Err err) ->
            let
                _ =
                    Debug.log "could not download modules" meta
            in
            ( model, Cmd.none )



-- COMMANDS


searchAllPackages : Cmd Msg
searchAllPackages =
    Http.get
        { url = packagesSearchUrl
        , expect = Http.expectJson GotSearch (Json.Decode.list packageMetaDecoder)
        }


downloadNext : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
downloadNext ( model, cmd ) =
    case model.packages of
        meta :: tail ->
            ( { model | packages = tail }
            , Cmd.batch
                [ cmd
                , fetchInfo meta
                , fetchReadme meta
                , fetchModules meta
                ]
            )

        [] ->
            let
                _ =
                    Debug.log "all" "complete"
            in
            ( model, cmd )


fetchInfo : PackageMeta -> Cmd Msg
fetchInfo meta =
    Http.get
        { url = packageInfoUrl meta
        , expect = Http.expectJson (GotInfo meta) Json.Decode.value
        }


fetchReadme : PackageMeta -> Cmd Msg
fetchReadme meta =
    Http.get
        { url = packageReadmeUrl meta
        , expect = Http.expectString (GotReadme meta)
        }


fetchModules : PackageMeta -> Cmd Msg
fetchModules meta =
    Http.get
        { url = packageModulesUrl meta
        , expect = Http.expectJson (GotModules meta) Json.Decode.value
        }


writeSearch : List PackageMeta -> Cmd msg
writeSearch packages =
    writeFile
        (Json.Encode.object
            [ ( "path", "search.json" |> Json.Encode.string )
            , ( "content", packages |> Json.Encode.list (packagePath "" >> Json.Encode.string) |> Json.Encode.encode 2 |> Json.Encode.string )
            ]
        )


writeInfo : PackageMeta -> Json.Encode.Value -> Cmd msg
writeInfo meta info =
    writeFile
        (Json.Encode.object
            [ ( "path", packagePath "elm.json" meta |> Json.Encode.string )
            , ( "content", info |> Json.Encode.encode 2 |> Json.Encode.string )
            ]
        )


writeReadme : PackageMeta -> String -> Cmd msg
writeReadme meta readme =
    writeFile
        (Json.Encode.object
            [ ( "path", packagePath "readme.json" meta |> Json.Encode.string )
            , ( "content", Json.Encode.object [ ( "readme", Json.Encode.string readme ) ] |> Json.Encode.encode 2 |> Json.Encode.string )
            ]
        )


writeModules : PackageMeta -> Json.Encode.Value -> Cmd msg
writeModules meta modules =
    writeFile
        (Json.Encode.object
            [ ( "path", packagePath "docs.json" meta |> Json.Encode.string )
            , ( "content", modules |> Json.Encode.encode 2 |> Json.Encode.string )
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- PORT


port writeFile : Json.Encode.Value -> Cmd msg



-- HELPERS


packagesSearchUrl : String
packagesSearchUrl =
    "https://package.elm-lang.org/search.json"


packageInfoUrl : PackageMeta -> String
packageInfoUrl =
    packageUrl "elm.json"


packageReadmeUrl : PackageMeta -> String
packageReadmeUrl =
    packageUrl "README.md"


packageModulesUrl : PackageMeta -> String
packageModulesUrl =
    packageUrl "docs.json"


packageUrl : String -> PackageMeta -> String
packageUrl file meta =
    "http://package.elm-lang.org/packages/" ++ packagePath file meta


packagePath : String -> PackageMeta -> String
packagePath file { name, version } =
    String.join "/"
        [ Elm.Package.toString name
        , Elm.Version.toString version
        , file
        ]
