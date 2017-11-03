module Docs.Package
    exposing
        ( Complete
        , Entry
        , Module
        , Partial
        , completeDecoder
        , identifier
        , remotePartialDecoder
        )

import Docs.Type exposing (Type)
import Elm.Documentation as ElmDocs
import Json.Decode as Decode exposing (Decoder)


type alias Partial =
    { user : String
    , name : String
    , version : String
    }


type alias Complete =
    { user : String
    , name : String
    , version : String
    , modules : List Module
    }


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


identifier : { a | user : String, name : String, version : String } -> String
identifier { user, name, version } =
    user ++ "/" ++ name ++ "/" ++ version


completeDecoder : String -> Partial -> Decode.Decoder Complete
completeDecoder elmVersion { user, name, version } =
    ElmDocs.decoder
        |> Decode.map (elmDocsToModule elmVersion)
        |> Decode.list
        |> Decode.map (Complete user name version)


remotePartialDecoder : Decode.Decoder Partial
remotePartialDecoder =
    Decode.map2 (,)
        (Decode.field "name" Decode.string)
        (Decode.field "versions" <| Decode.index 0 Decode.string)
        |> Decode.andThen remotePartialDecoderHelp


remotePartialDecoderHelp : ( String, String ) -> Decode.Decoder Partial
remotePartialDecoderHelp ( fullName, version ) =
    case String.split "/" fullName of
        [ user, name ] ->
            Decode.succeed <| Partial user name version

        _ ->
            Decode.fail "package names must look like `user/project`"


elmDocsToModule : String -> ElmDocs.Documentation -> Module
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
