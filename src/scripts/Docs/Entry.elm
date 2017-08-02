module Docs.Entry exposing (..)

import Docs.Type as Type exposing (Type)
import Json.Decode as Decode exposing (Decoder)


type alias Entry =
    { name : String
    , docs : String
    , tipe : Type
    }


decoder : Decoder Entry
decoder =
    Decode.map3 Entry
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "type" Type.decoder)
