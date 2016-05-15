module Package.Package exposing (..)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Module as Module exposing (Module)
import Package.Module.Name as Name
import Package.Version as Version exposing (Version)


type alias Package =
    { name : String
    , version : Version
    , modules : List Module
    }


decoder : Decoder Package
decoder =
    Decode.object3 Package
        ("name" := Decode.string)
        ("version" := Version.decoder)
        ("docs" := Decode.list Module.decoder)
