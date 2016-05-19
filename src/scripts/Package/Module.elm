module Package.Module exposing (..)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Module.Name as Name exposing (Name)
import Package.Module.Entry as Entry exposing (Entry)
import Package.Version as Version exposing (Version)


type alias Module =
    { name : String
    , entries : List Entry
    , elmVersion : Maybe Version
    }


decoder : Decoder Module
decoder =
    Decode.object3 Module
        ("name" := Decode.string)
        ("values" := Decode.list Entry.decoder)
        ("generated-with-elm-version"
            := Decode.oneOf
                [ Decode.map Just Version.decoder
                , Decode.succeed Nothing
                ]
        )
