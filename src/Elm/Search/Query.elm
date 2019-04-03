module Elm.Search.Query exposing (Query(..), fromString)

import Char
import Elm.Type exposing (Type)
import Json.Encode


type PartialType
    = PartialType


type Query
    = ByType Type
    | ByPartialType PartialType
    | ByName String
    | ByText String


fromString : String -> List Query
fromString string =
    List.filterMap identity
        [ typeFromString string |> Maybe.map ByType
        , partialTypeFromString string |> Maybe.map ByPartialType
        , nameFromString string |> Maybe.map ByName
        , textFromString string |> Maybe.map ByText
        ]


typeFromString : String -> Maybe Type
typeFromString =
    Elm.Type.parse >> Result.toMaybe


partialTypeFromString : String -> Maybe PartialType
partialTypeFromString _ =
    Nothing


nameFromString : String -> Maybe String
nameFromString string =
    if isName string then
        Just string

    else
        Nothing


textFromString : String -> Maybe String
textFromString string =
    if isText string then
        Just string

    else
        Nothing


isName : String -> Bool
isName =
    String.all
        (\char -> Char.isAlphaNum char || char == '.' || char == '/')


isText : String -> Bool
isText string =
    String.contains " " string || not (isName string)
