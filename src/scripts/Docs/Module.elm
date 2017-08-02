module Docs.Module exposing (..)

import Docs.Entry as Entry exposing (Entry)
import Docs.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)


type alias Module =
    { name : String
    , entries : List Entry
    , elmVersion : Maybe Version
    }


decoder : Decoder Module
decoder =
    Decode.map3 Module
        (Decode.field "name" Decode.string)
        (Decode.field "values" (Decode.list Entry.decoder))
        (Decode.field "generated-with-elm-version"
            (Decode.oneOf
                [ Decode.map Just Version.decoder
                , Decode.succeed Nothing
                ]
            )
        )
