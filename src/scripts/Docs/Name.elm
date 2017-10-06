module Docs.Name exposing (Name, decoder, fromString, pathTo)

import Json.Decode as Decode


type alias Name =
    { name : String
    , home : String
    }


decoder : Decode.Decoder Name
decoder =
    Decode.string
        |> Decode.andThen (fromString >> parseToDecoder)


fromString : String -> Maybe Name
fromString str =
    case List.reverse (String.split "." str) of
        name :: homeSegements ->
            List.reverse homeSegements
                |> String.join "."
                |> Name name
                |> Just

        _ ->
            Nothing


parseToDecoder : Maybe Name -> Decode.Decoder Name
parseToDecoder =
    let
        errorMessage =
            "names look like `maybe_nested.module.name`"
    in
    Maybe.map Decode.succeed
        >> Maybe.withDefault (Decode.fail errorMessage)


pathTo : Name -> String
pathTo { home, name } =
    String.map dotToDash home ++ "#" ++ name


dotToDash : Char -> Char
dotToDash char =
    if char == '.' then
        '-'
    else
        char
