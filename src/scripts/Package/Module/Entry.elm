module Package.Module.Entry exposing (..)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Module.Type as Type exposing (Type)


type alias Entry =
    { name : String
    , docs : String
    , info : Info
    }


type Info
    = Value Type (Maybe Fixity)
    | Union
        { vars : List String
        , tags : List Tag
        }
    | Alias
        { vars : List String
        , tipe : Type
        }


type alias Fixity =
    { precedence : Int
    , associativity : String
    }


type alias Tag =
    { tag : String
    , args : List Type
    }


decoder : Decoder Info -> Decoder Entry
decoder infoDecoder =
    Decode.object3 Entry
        ("name" := Decode.string)
        ("comment" := Decode.string)
        infoDecoder


valueDecoder : Decoder Info
valueDecoder =
    Decode.object2 Value
        ("type" := Type.decoder)
        (Decode.maybe fixityDecoder)


unionDecoder : Decoder Info
unionDecoder =
    Decode.object2 (\vars tags -> Union { vars = vars, tags = tags })
        ("args" := Decode.list Decode.string)
        ("cases" := Decode.list tagDecoder)


aliasDecoder : Decoder Info
aliasDecoder =
    Decode.object2 (\vars tipe -> Alias { vars = vars, tipe = tipe })
        ("args" := Decode.list Decode.string)
        ("type" := Type.decoder)


fixityDecoder : Decoder Fixity
fixityDecoder =
    Decode.object2 Fixity
        ("precedence" := Decode.int)
        ("associativity" := Decode.string)


tagDecoder : Decoder Tag
tagDecoder =
    Decode.tuple2 Tag Decode.string (Decode.list Type.decoder)
