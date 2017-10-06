module Docs.Version exposing (Version, decoder, fromRaw, toString)

import Json.Decode as Decode


type Version
    = Semantic Int Int Int


fromRaw : Int -> Int -> Int -> Version
fromRaw =
    Semantic


toString : Version -> String
toString (Semantic major minor patch) =
    Basics.toString major
        ++ "."
        ++ Basics.toString minor
        ++ "."
        ++ Basics.toString patch


decoder : Decode.Decoder Version
decoder =
    Decode.andThen decoderHelp Decode.string


decoderHelp : String -> Decode.Decoder Version
decoderHelp str =
    case List.map String.toInt (String.split "." str) of
        [ Ok major, Ok minor, Ok patch ] ->
            Decode.succeed (Semantic major minor patch)

        _ ->
            Decode.fail "versions look like `major.minor.patch`"
