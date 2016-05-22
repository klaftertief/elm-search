module Package.Entry exposing (..)

import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Type as Type exposing (Type)


type alias Entry =
    { name : String
    , docs : String
    , tipe : Type
    }


decoder : Decoder Entry
decoder =
    Decode.object3 Entry
        ("name" := Decode.string)
        ("comment" := Decode.string)
        ("type" := Type.decoder)
