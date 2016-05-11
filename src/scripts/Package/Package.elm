module Package.Package exposing (..)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Module as Module exposing (Module)
import Package.Version as Version exposing (Version)


type alias Package =
    { name : String
    , version : Version
    , modules : Dict.Dict String Module
    }


decoder : Decoder Package
decoder =
    Decode.object3 Package
        ("name" := Decode.string)
        ("version" := Version.decoder)
        ("docs" := Decode.map (dictBy .name) (Decode.list Module.decoder))


dictBy : (a -> comparable) -> List a -> Dict.Dict comparable a
dictBy f list =
    Dict.fromList (List.map (\x -> ( f x, x )) list)
