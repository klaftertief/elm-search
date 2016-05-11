module Package.Module.Name exposing (..)

-- where

import Json.Decode as Decode exposing (Decoder, (:=))
import String


type alias Name =
    { home : String
    , name : String
    }


nameToString : Name -> String
nameToString { home, name } =
    String.join "." [ home, name ]


decoder : Decoder Name
decoder =
    Decode.customDecoder Decode.string fromString


fromString : String -> Result String Name
fromString str =
    case (List.reverse (String.split "." str)) of
        name :: home ->
            Ok (Name name (List.reverse home |> String.join "."))

        _ ->
            Err (str ++ " is not a valid Elm name")


pathTo : Name -> String
pathTo { home, name } =
    String.map dotToDash
        home
        ++ "#"
        ++ name


dotToDash : Char -> Char
dotToDash char =
    if char == '.' then
        '-'
    else
        char
