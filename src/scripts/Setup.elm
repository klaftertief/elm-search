port module Setup exposing (main)

import Blacklist
import Docs.Package as Package
import Docs.Package.Cache as Cache
import Generate
import Http
import Json.Decode as Decode
import Set
import Task


type alias Model =
    { elmVersion : String
    }


type Msg
    = All (List Package.Metadata)
    | CacheMiss Package.Metadata
    | Response Package.Package


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        elmVersion =
            "0.19.1"

        getAllPackages =
            Decode.list Package.remoteMetadataDecoder
                |> Http.get "https://package.elm-lang.org/search.json"
                |> Http.toTask
    in
    ( Model elmVersion
    , getAllPackages
        |> Task.attempt (ensureOk All)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        All partials ->
            ( model, planFileWrites partials )

        CacheMiss metadata ->
            ( model, fetchDocs model.elmVersion metadata )

        Response package ->
            ( model, cacheModule package )



-- COMMANDS


planFileWrites : List Package.Metadata -> Cmd Msg
planFileWrites partials =
    let
        moduleNames =
            List.map safeModuleName partials
    in
    writeOutput (Generate.main_ moduleNames)
        :: List.map2 Cache.check moduleNames partials
        |> Cmd.batch


fetchDocs : String -> Package.Metadata -> Cmd Msg
fetchDocs elmVersion metadata =
    if Blacklist.contains metadata then
        Cmd.none

    else
        let
            url =
                String.join "/"
                    [ "https://package.elm-lang.org/packages"
                    , Package.identifier metadata
                    , "docs.json"
                    ]

            decoder =
                Package.decode elmVersion metadata
        in
        Http.get url decoder
            |> Http.send (ensureOk Response)


cacheModule : Package.Package -> Cmd msg
cacheModule package =
    let
        name =
            safeModuleName package.metadata
    in
    Cache.put
        { moduleName = name
        , code = Generate.package name package
        }


safeModuleName : Package.Metadata -> String
safeModuleName { user, name, version } =
    "M_" ++ Generate.lowerName (user ++ "__" ++ name ++ "__" ++ version)


replaceUnsafe : Char -> Char
replaceUnsafe char =
    if char == '-' || char == '.' then
        '_'

    else
        char


ensureOk : (a -> b) -> Result x a -> b
ensureOk func result =
    case result of
        Ok value ->
            func value

        Err e ->
            Debug.todo (Debug.toString e)


subscriptions : Model -> Sub Msg
subscriptions model =
    Cache.onMissing CacheMiss



-- PORT


port writeOutput : String -> Cmd msg
