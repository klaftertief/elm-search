port module Setup exposing (..)

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


type alias Model =
    { packages : List Package
    , remaining : Maybe Int
    , version : Version
    }


type Msg
    = All (List Package)
    | Next Package


port done : String -> Cmd msg


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing ( 0, 18, 0 )
    , Decode.list decodeEmptyPackage
        |> Http.get "http://package.elm-lang.org/all-packages?elm-package-version=0.18"
        |> Http.send (ensureOk All)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.remaining ) of
        ( All packages, _ ) ->
            ( { model
                | remaining =
                    Just (List.length packages - Blacklist.length)
              }
            , Cmd.batch (List.map (fetchModules model.version) packages)
            )

        ( Next package, Just 1 ) ->
            ( model
            , done <| Generate.elm <| package :: model.packages
            )

        ( Next package, _ ) ->
            ( { model
                | packages = package :: model.packages
                , remaining = Maybe.map dec model.remaining
              }
            , Cmd.none
            )


fetchModules : Version -> Package -> Cmd Msg
fetchModules version package =
    let
        url =
            "http://package.elm-lang.org/packages/"
                ++ package.user
                ++ "/"
                ++ package.name
                ++ "/"
                ++ Docs.Version.vsnToString package.version
                ++ "/documentation.json"
    in
    if Blacklist.contains package then
        Cmd.none
    else
        Http.get url (decodePackageModules version package)
            |> Http.send (ensureOk Next)


decodeEmptyPackage : Decode.Decoder Package
decodeEmptyPackage =
    Decode.map2 (,)
        (Decode.field "name" Decode.string)
        (Decode.field "versions" <| Decode.index 0 Docs.Version.decoder)
        |> Decode.andThen decodeEmptyPackageHelp


decodeEmptyPackageHelp : ( String, Version ) -> Decode.Decoder Package
decodeEmptyPackageHelp ( fullName, version ) =
    case String.split "/" fullName of
        [ user, name ] ->
            Decode.succeed <| Package user name version []

        _ ->
            Decode.fail "names must look like `user/project`"


decodePackageModules : Version -> Package -> Decode.Decoder Package
decodePackageModules version package =
    ElmDocs.decoder
        |> Decode.map (elmDocsToModule version)
        |> Decode.list
        |> Decode.map (\modules -> { package | modules = modules })


elmDocsToModule : Version -> ElmDocs.Documentation -> Module
elmDocsToModule version { name, values } =
    Module name (List.map elmValueToEntry values) (Just version)


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


dec : Int -> Int
dec x =
    x - 1
