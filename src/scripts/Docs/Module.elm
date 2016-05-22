module Docs.Module exposing (..)

import Json.Decode as Decode exposing (Decoder, (:=))
import Docs.Entry as Entry exposing (Entry)
import Docs.Version as Version exposing (Version)


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
