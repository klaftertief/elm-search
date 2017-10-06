module Docs.Package
    exposing
        ( Entry
        , Module
        , Package
        , empty
        , identifier
        , remoteMetaDataDecoder
        , simpleDecoder
        , simpleEncoder
        , withModulesDecoder
        )

import Docs.Type exposing (Type)
import Docs.Version exposing (Version)
import Elm.Documentation as ElmDocs
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Package =
    { user : String
    , name : String
    , version : Version
    , modules : List Module
    }


type alias Module =
    { name : String
    , entries : List Entry
    , elmVersion : Maybe Version
    }


type alias Entry =
    { name : String
    , docs : String
    , tipe : Type
    }


identifier : Package -> String
identifier { user, name, version } =
    String.join "/" [ user, name, Docs.Version.toString version ]


empty : Encode.Value
empty =
    Encode.list []


simpleEncoder : Package -> Encode.Value -> Encode.Value
simpleEncoder { user, name, version } rawModules =
    Encode.object
        [ ( "user", Encode.string user )
        , ( "name", Encode.string name )
        , ( "version", Encode.string <| Docs.Version.toString version )
        , ( "modules", rawModules )
        ]


simpleDecoder : Version -> Decoder Package
simpleDecoder elmVersion =
    Decode.map4 Package
        (Decode.field "user" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "version" Docs.Version.decoder)
        (Decode.succeed [])
        |> Decode.andThen
            (Decode.field "modules" << withModulesDecoder elmVersion)


withModulesDecoder : Version -> Package -> Decode.Decoder Package
withModulesDecoder elmVersion { user, name, version } =
    ElmDocs.decoder
        |> Decode.map (elmDocsToModule elmVersion)
        |> Decode.list
        |> Decode.map (Package user name version)


remoteMetaDataDecoder : Decode.Decoder Package
remoteMetaDataDecoder =
    Decode.map2 (,)
        (Decode.field "name" Decode.string)
        (Decode.field "versions" <| Decode.index 0 Docs.Version.decoder)
        |> Decode.andThen remoteMetaDataDecoderHelp


remoteMetaDataDecoderHelp : ( String, Version ) -> Decode.Decoder Package
remoteMetaDataDecoderHelp ( fullName, version ) =
    case String.split "/" fullName of
        [ user, name ] ->
            Decode.succeed <| Package user name version []

        _ ->
            Decode.fail "package names must look like `user/project`"


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
