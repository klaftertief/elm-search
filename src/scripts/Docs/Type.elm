module Docs.Type
    exposing
        ( Type(..)
        , decoder
        , normalize
        , parse
        , reserverdVars
        )

import Char
import Dict exposing (Dict)
import Docs.Name as Name exposing (Name)
import Elm.Documentation.Type as Type
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Type
    = Function (List Type) Type
    | Var String
    | Apply Name (List Type)
    | Tuple (List Type)
    | Record (List ( String, Type )) (Maybe String)


parse : String -> Result String Type
parse =
    Decode.decodeValue decoder << Encode.string


decoder : Decoder Type
decoder =
    Decode.andThen (Decode.succeed << toInternal []) Type.decoder


toInternal : List Type -> Type.Type -> Type
toInternal functionArgs elmType =
    case elmType of
        Type.Var name ->
            Var name

        Type.Lambda first ((Type.Lambda _ _) as next) ->
            toInternal (toInternal [] first :: functionArgs) next

        Type.Lambda almostLast last ->
            Function
                (List.reverse (toInternal [] almostLast :: functionArgs))
                (toInternal [] last)

        Type.Tuple args ->
            Tuple (List.map (toInternal []) args)

        Type.Type name args ->
            Apply (toName name) (List.map (toInternal []) args)

        Type.Record args extensible ->
            Record (List.map (Tuple.mapSecond (toInternal [])) args) extensible


toName : String -> Name.Name
toName str =
    Name.fromString str
        |> Result.withDefault { name = str, home = "" }



-- NORMALIZE


reserverdVars : Dict String (List String)
reserverdVars =
    Dict.empty
        |> Dict.insert "number" [ "Float", "Int" ]
        |> Dict.insert "comparable" [ "Float", "Int", "Char", "String" ]
        |> Dict.insert "appendable" [ "String", "List" ]


type alias Mapping =
    Dict String String


defaultMapping : Mapping
defaultMapping =
    Dict.keys reserverdVars
        |> List.map (\v -> ( v, v ))
        |> Dict.fromList


nextMappingValue : Mapping -> String
nextMappingValue mapping =
    let
        base =
            Dict.size mapping - Dict.size defaultMapping

        code =
            (base % 26) + Char.toCode 'a'

        string =
            String.fromChar (Char.fromCode code)

        times =
            (base // 26) + 1
    in
    String.repeat times string


updateMapping : Type -> Mapping -> Mapping
updateMapping tipe mapping =
    let
        updateMappingFor name =
            if Dict.member name mapping then
                mapping
            else
                Dict.insert name
                    (nextMappingValue mapping)
                    mapping
    in
    case tipe of
        Function args result ->
            List.foldl updateMapping mapping (List.append args [ result ])

        Var name ->
            updateMappingFor name

        Apply name args ->
            List.foldl updateMapping mapping args

        Tuple args ->
            List.foldl updateMapping mapping args

        Record fields ext ->
            List.foldl updateMapping mapping (List.map (\( _, t ) -> t) fields)


normalize : Type -> Type
normalize tipe =
    normalizeWithMapping (updateMapping tipe defaultMapping) tipe


normalizeWithMapping : Mapping -> Type -> Type
normalizeWithMapping mapping tipe =
    let
        normalize_ =
            normalizeWithMapping mapping
    in
    case tipe of
        Function args result ->
            Function (List.map normalize_ args)
                (normalize_ result)

        Var name ->
            let
                name_ =
                    case Dict.get name mapping of
                        Just n ->
                            n

                        Nothing ->
                            name
            in
            Var name_

        Apply name args ->
            Apply name (List.map normalize_ args)

        Tuple args ->
            Tuple (List.map normalize_ args)

        Record fields ext ->
            Record (List.map (\( k, v ) -> ( k, normalize_ v )) fields) ext
