module Package.Package exposing (..)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Module as Module exposing (Module)
import Package.Version as Version exposing (Version)
import String


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
        Decode.object3 make
            ("name" := Decode.customDecoder Decode.string userNameFromString)
            ("version" := Version.decoder)
            ("docs" := Decode.list Module.decoder)


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
