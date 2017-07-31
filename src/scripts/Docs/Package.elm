module Docs.Package exposing (..)

import Docs.Module as Module exposing (Module)
import Docs.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import String
import Utils.Json


type alias Package =
    { user : String
    , name : String
    , version : Version
    , modules : List Module
    }


decoder : Decoder Package
decoder =
    let
        make ( user, name ) version modules =
            Package user name version modules
    in
    Decode.map3 make
        (Decode.field "name" (Utils.Json.customDecoder Decode.string userNameFromString))
        (Decode.field "version" Version.decoder)
        (Decode.field "docs" (Decode.list Module.decoder))


userNameFromString : String -> Result String ( String, String )
userNameFromString str =
    case String.split "/" str of
        [ user, name ] ->
            Result.Ok ( user, name )

        _ ->
            Result.Err ("`" ++ str ++ "` is not a valid package name.")


identifier : Package -> String
identifier { user, name, version } =
    String.join "/" [ user, name, Version.vsnToString version ]
