module Package.Module exposing (..)

-- where

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, (:=))
import Package.Module.Name as Name exposing (Name)
import Package.Module.Entry as Entry exposing (Entry)


type alias Module =
  { name : Name
  , entries : Dict String Entry
  }


decoder : Decoder Module
decoder =
  let
    make name values unions aliases =
      Module name (dictBy .name (values ++ unions ++ aliases))
  in
    Decode.object4
      make
      ("name" := Name.decoder)
      ("aliases" := Decode.list (Entry.decoder Entry.aliasDecoder))
      ("types" := Decode.list (Entry.decoder Entry.unionDecoder))
      ("values" := Decode.list (Entry.decoder Entry.valueDecoder))


dictBy : (a -> comparable) -> List a -> Dict.Dict comparable a
dictBy f list =
  Dict.fromList (List.map (\x -> ( f x, x )) list)
