module Elm.Search.Query exposing (Query(..), fromString)

import Char
import Elm.Type exposing (Type)
import Elm.Type.Partial as Partial
import Json.Encode


type Query
    = ByType Type
    | ByPartialType Partial.Type
    | ByName String
    | ByText String


fromString : String -> List Query
fromString string =
    List.filterMap identity
        -- [ typeFromString string |> Maybe.map ByType
        -- , partialTypeFromString string |> Maybe.map ByPartialType
        [ partialTypeFromString string |> Maybe.map ByPartialType
        , nameFromString string |> Maybe.map ByName
        , textFromString string |> Maybe.map ByText
        ]


typeFromString : String -> Maybe Type
typeFromString =
    Elm.Type.parse >> Result.toMaybe


partialTypeFromString : String -> Maybe Partial.Type
partialTypeFromString =
    Partial.parse >> Result.toMaybe


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
