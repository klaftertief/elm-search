module Package.Module exposing (..)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Module.Name as Name exposing (Name)
import Package.Module.Entry as Entry exposing (Entry)


type alias Module =
    { name : Name
    , entries : List Entry
    }


decoder : Decoder Module
decoder =
    Decode.object2 Module
        ("name" := Name.decoder)
        ("values" := Decode.list Entry.decoder)
