module Utils.Json exposing (..)

import Json.Decode as Decode


customDecoder : Decode.Decoder a -> (a -> Result String b) -> Decode.Decoder b
customDecoder decoder f =
    let
        customDecoderHelp a =
            case f a of
                Ok b ->
                    Decode.succeed b

                Err reason ->
                    Decode.fail reason
    in
    Decode.andThen customDecoderHelp decoder
