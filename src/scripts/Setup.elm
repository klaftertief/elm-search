port module Setup exposing (main)

import Blacklist
import Docs.Package as Package
import Docs.Package.Cache as Cache
import Generate
import Http
import Json.Decode as Decode


type alias Model =
    { elmVersion : String
    }


type Msg
    = All (Result Http.Error (List Package.Metadata))
    | CacheMiss Package.Metadata
    | Response String (Result Http.Error Package.Package)


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
            "0.19.0"

        getAllPackages =
            Decode.list Package.remoteMetadataDecoder
                |> Http.get "https://package.elm-lang.org/search.json"
                |> Http.send All
    in
    ( Model elmVersion
    , getAllPackages
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        All (Ok partials) ->
            ( model, planFileWrites partials )

        All (Err err) ->
            ( model, Cmd.none )

        CacheMiss metadata ->
            ( model, fetchDocs model.elmVersion metadata )

        Response _ (Ok package) ->
            ( model, cacheModule package )

        Response _ (Err (Http.BadPayload err { url })) ->
            ( model, log (url ++ ": " ++ err) )

        Response _ (Err (Http.BadStatus { url })) ->
            ( model, log ("Bad Status: " ++ url) )

        Response _ (Err (Http.BadUrl url)) ->
            ( model, log ("Bad Url: " ++ url) )

        Response url (Err Http.Timeout) ->
            ( model, log ("Timeout: " ++ url) )

        Response url (Err Http.NetworkError) ->
            ( model, log ("NetworkError: " ++ url) )



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
                    [ "http://package.elm-lang.org/packages"
                    , Package.identifier metadata
                    , "docs.json"
                    ]

            decoder =
                Package.decode elmVersion metadata
        in
        Http.get url decoder
            |> Http.send (Response url)


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Cache.onMissing CacheMiss



-- PORT


port writeOutput : String -> Cmd msg


port log : String -> Cmd msg
