module Query exposing (Query(..))

import Elm.Type exposing (Type)
import Json.Decode
import Json.Encode


type Query
    = PackageByAuthor String
    | PackageByName String
    | ModuleByName String
    | ModuleByQualifiedName (List String) String
    | TypeByName String
    | ValueByName String
    | ValueByType Type


fromString : String -> List Query
fromString queryString =
    let
        toQuery : (String -> Maybe Query) -> Maybe Query
        toQuery f =
            f queryString
    in
    List.filterMap toQuery
        [ valueByType
        ]


valueByName : String -> Maybe Query
valueByName queryString =
    queryString
        |> Json.Encode.string
        |> Json.Decode.decodeValue Elm.Type.decoder
        |> Result.toMaybe
        |> Maybe.map ValueByType


valueByType : String -> Maybe Query
valueByType queryString =
    queryString
        |> Json.Encode.string
        |> Json.Decode.decodeValue Elm.Type.decoder
        |> Result.toMaybe
        |> Maybe.map ValueByType
