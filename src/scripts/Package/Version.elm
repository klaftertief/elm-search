module Package.Version exposing (Version, decoder, vsnToString)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import String


type alias Version =
    ( Int, Int, Int )


vsnToString : Version -> String
vsnToString ( major, minor, patch ) =
    toString major ++ "." ++ toString minor ++ "." ++ toString patch


decoder : Decoder Version
decoder =
    Decode.customDecoder Decode.string fromString


fromString : String -> Result String Version
fromString str =
    case all (List.map String.toInt (String.split "." str)) of
        Ok [ major, minor, patch ] ->
            Ok ( major, minor, patch )

        _ ->
            Err (str ++ " is not a valid Elm version")


all : List (Result x a) -> Result x (List a)
all list =
    case list of
        [] ->
            Ok []

        x :: xs ->
            Result.map2 (::) x (all xs)
