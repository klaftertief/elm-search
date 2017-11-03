port module Setup exposing (main)

import Blacklist
import Docs.Package as Package
import Docs.Package.Cache as Cache
import Http
import Json.Decode as Decode
import Set
import Task


type alias Model =
    { elmVersion : String
    }


type Msg
    = All (List Package.Partial)
    | CacheMiss Package.Partial
    | Response Package.Complete


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
            Decode.list Package.remotePartialDecoder
                |> Http.get "http://package.elm-lang.org/all-packages"
                |> Http.toTask
    in
    ( Model elmVersion
    , Task.map2 onlyNewPackages getNewPackageNames getAllPackages
        |> Task.attempt (ensureOk All)
    )


onlyNewPackages : List String -> List Package.Partial -> List Package.Partial
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
        All partials ->
            ( model, planFileWrites partials )

        CacheMiss partial ->
            ( model, fetchDocs model.elmVersion partial )

        Response complete ->
            ( model, cacheModule complete )



-- COMMANDS


planFileWrites : List Package.Partial -> Cmd Msg
planFileWrites partials =
    let
        moduleNames =
            List.map safeModuleName partials
    in
    writeOutput (mainModuleCode moduleNames)
        :: List.map2 Cache.check moduleNames partials
        |> Cmd.batch


fetchDocs : String -> Package.Partial -> Cmd Msg
fetchDocs elmVersion partial =
    if Blacklist.contains partial then
        Cmd.none
    else
        let
            url =
                String.join "/"
                    [ "http://package.elm-lang.org/packages"
                    , Package.identifier partial
                    , "documentation.json"
                    ]

            decoder =
                Package.completeDecoder elmVersion partial
        in
        Http.get url decoder
            |> Http.send (ensureOk Response)


cacheModule : Package.Complete -> Cmd msg
cacheModule complete =
    let
        name =
            safeModuleName complete
    in
    Cache.put
        { moduleName = name
        , code = packageModuleCode name complete
        }


safeModuleName : { a | user : String, name : String, version : String } -> String
safeModuleName { user, name, version } =
    let
        combined =
            user ++ "__" ++ name ++ "__" ++ version
    in
    "M_" ++ String.map replaceUnsafe combined


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

        Err _ ->
            Debug.crash "fatal error"


subscriptions : Model -> Sub Msg
subscriptions model =
    Cache.onMissing CacheMiss



-- CODE


mainModuleCode : List String -> String
mainModuleCode dependencies =
    let
        imports =
            List.map (\dep -> "import " ++ dep) ("Web" :: dependencies)
                |> String.join "\n"

        packageList =
            List.map (\dep -> dep ++ ".package") dependencies
                |> (\elements -> "[" ++ String.join "," elements ++ "]")
    in
    imports ++ "\n\nmain = Web.program " ++ packageList


packageModuleCode : String -> Package.Complete -> String
packageModuleCode name complete =
    String.join "\n"
        [ "module " ++ name ++ " exposing(package)"
        , "import Docs.Type exposing(..)"
        , "package = " ++ toString complete
        ]



-- PORT


port writeOutput : String -> Cmd msg
