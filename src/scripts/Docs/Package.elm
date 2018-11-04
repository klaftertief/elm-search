module Docs.Package exposing
    ( Entry
    , Metadata
    , Module
    , Package
    , decode
    , identifier
    , metadataToString
    , remoteMetadataDecoder
    )

import Docs.Type exposing (Type)
import Elm.Docs as ElmDocs
import Json.Decode as Decode exposing (Decoder)


type alias Package =
    { metadata : Metadata
    , modules : List Module
    }


type alias Metadata =
    { user : String
    , name : String
    , version : String
    }


metadataToString : Metadata -> String
metadataToString metadata =
    "{ user = \""
        ++ metadata.user
        ++ "\", name = \""
        ++ metadata.name
        ++ "\", version = \""
        ++ metadata.version
        ++ "\"}"


type alias Module =
    { name : String
    , entries : List Entry
    , elmVersion : Maybe String
    }


type alias Entry =
    { name : String
    , docs : String
    , tipe : Type
    }


identifier : Metadata -> String
identifier { user, name, version } =
    user ++ "/" ++ name ++ "/" ++ version


decode : String -> Metadata -> Decode.Decoder Package
decode elmVersion metadata =
    ElmDocs.decoder
        |> Decode.map (elmDocsToModule elmVersion)
        |> Decode.list
        |> Decode.map (Package metadata)


remoteMetadataDecoder : Decode.Decoder Metadata
remoteMetadataDecoder =
    Decode.map2 (\a b -> ( a, b ))
        (Decode.field "name" Decode.string)
        (Decode.field "versions" <| Decode.index 0 Decode.string)
        |> Decode.andThen remoteMetadataHelp


remoteMetadataHelp : ( String, String ) -> Decode.Decoder Metadata
remoteMetadataHelp ( fullName, version ) =
    case String.split "/" fullName of
        [ user, name ] ->
            Decode.succeed <| Metadata user name version

        _ ->
            Decode.fail "package names must look like `user/project`"


elmDocsToModule : String -> ElmDocs.Module -> Module
elmDocsToModule elmVersion { name, values } =
    Module name (List.map elmValueToEntry values) (Just elmVersion)


elmValueToEntry : ElmDocs.Value -> Entry
elmValueToEntry { name, tipe, comment } =
    Entry name comment (Docs.Type.toInternal tipe)
