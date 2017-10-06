port module Setup exposing (main)

import Blacklist
import Docs.Entry exposing (Entry)
import Docs.Module exposing (Module)
import Docs.Package exposing (Package)
import Docs.Type
import Docs.Version exposing (Version)
import Elm.Documentation as ElmDocs
import Generate
import Http
import Json.Decode as Decode
import Set
import Task


type alias Model =
    { packages : List Package
    , remaining : Int
    , elmVersion : Version
    }


type Msg
    = All (List Partial)
    | Response Decode.Value Package
    | Local Decode.Value Partial
    | Fetch Partial


type alias Partial =
    { user : String
    , name : String
    , version : Version
    }


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    let
        elmVersion =
            ( 0, 18, 0 )

        getNewPackageNames =
            Decode.list Decode.string
                |> Http.get "http://package.elm-lang.org/new-packages"
                |> Http.toTask

        getAllPackages =
            Decode.list decodePartial
                |> Http.get "http://package.elm-lang.org/all-packages"
                |> Http.toTask
    in
    ( Model [] -1 elmVersion
    , Task.map2 onlyNewPackages getNewPackageNames getAllPackages
        |> Task.attempt (ensureOk All)
    )


onlyNewPackages : List String -> List Partial -> List Partial
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
            let
                newModel =
                    { model | remaining = List.length partials - Blacklist.length }
            in
            ( newModel, Cmd.batch (List.map checkCache partials) )

        Response json package ->
            let
                newModel =
                    addToState package model
            in
            ( newModel, Cmd.batch [ cache json, attemptFinish newModel ] )

        Local json partial ->
            let
                newModel =
                    addToState (localModules model.elmVersion partial json) model
            in
            ( newModel, attemptFinish newModel )

        Fetch partial ->
            ( model, fetchModules model.elmVersion partial )


addToState : Package -> Model -> Model
addToState package model =
    { model
        | packages = package :: model.packages
        , remaining = model.remaining - 1
    }


attemptFinish : Model -> Cmd Msg
attemptFinish { remaining, packages } =
    if remaining == 0 then
        done <| Generate.elm packages
    else
        Cmd.none


fetchModules : Version -> Partial -> Cmd Msg
fetchModules elmVersion partial =
    let
        url =
            String.join "/"
                [ "http://package.elm-lang.org/packages"
                , partial.user
                , partial.name
                , Docs.Version.vsnToString partial.version
                , "documentation.json"
                ]
    in
    if Blacklist.contains partial then
        Cmd.none
    else
        Http.get url (decodePackageModules elmVersion partial)
            |> Http.send (ensureOk Response)


localModules : Version -> Partial -> Decode.Value -> Package
localModules elmVersion partial json =
    let
        packageDecoder =
            decodePackageModules elmVersion partial
    in
    Decode.decodeValue packageDecoder json |> ensureOk


decodePartial : Decode.Decoder Partial
decodePartial =
    Decode.map2 (,)
        (Decode.field "name" Decode.string)
        (Decode.field "versions" <| Decode.index 0 Docs.Version.decoder)
        |> Decode.andThen decodePartialHelp


decodePartialHelp : ( String, Version ) -> Decode.Decoder Partial
decodePartialHelp ( fullName, version ) =
    case String.split "/" fullName of
        [ user, name ] ->
            Decode.succeed <| Partial user name version

        _ ->
            Decode.fail "names must look like `user/project`"


decodePackageModules : Version -> Partial -> Decode.Decoder Package
decodePackageModules elmVersion { user, name, version } =
    ElmDocs.decoder
        |> Decode.map (elmDocsToModule elmVersion)
        |> Decode.list
        |> Decode.map (Package user name version)


elmDocsToModule : Version -> ElmDocs.Documentation -> Module
elmDocsToModule elmVersion { name, values } =
    Module name (List.map elmValueToEntry values) (Just elmVersion)


elmValueToEntry : ElmDocs.Value -> Entry
elmValueToEntry { name, tipe, comment } =
    let
        internalName =
            case name of
                ElmDocs.Name str ->
                    str

                ElmDocs.Op str _ _ ->
                    str
    in
    Entry internalName comment (Docs.Type.toInternal tipe)


ensureOk : (a -> b) -> Result x a -> b
ensureOk func result =
    case result of
        Ok value ->
            func value

        Err x ->
            Debug.crash <| toString x


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fromCache (uncurry Local)
        , fromServer Fetch
        ]


port done : String -> Cmd msg


port cache : Decode.Value -> Cmd msg


port checkCache : Partial -> Cmd msg


port fromCache : (( Decode.Value, Partial ) -> msg) -> Sub msg


port fromServer : (Partial -> msg) -> Sub msg
