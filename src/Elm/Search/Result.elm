module Elm.Search.Result exposing
    ( Block(..)
    , ModuleIdentifier
    , PackageIdentifier
    , blockDecoder
    , elmTypeToString
    , encodeBlock
    )

import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type
import Elm.Version
import Json.Decode
import Json.Encode


type Block
    = Package PackageIdentifier Elm.Project.PackageInfo
    | Module PackageIdentifier ModuleIdentifier Elm.Docs.Module
    | Union PackageIdentifier ModuleIdentifier Elm.Docs.Union
    | Alias PackageIdentifier ModuleIdentifier Elm.Docs.Alias
    | Value PackageIdentifier ModuleIdentifier Elm.Docs.Value
    | Binop PackageIdentifier ModuleIdentifier Elm.Docs.Binop


blockDecoder : Json.Decode.Decoder Block
blockDecoder =
    Json.Decode.oneOf
        [ aliasBlockDecoder
        , unionBlockDecoder
        , valueBlockDecoder
        , binopBlockDecoder
        ]


unionBlockDecoder : Json.Decode.Decoder Block
unionBlockDecoder =
    Json.Decode.field "union" Elm.Docs.unionDecoder
        |> Json.Decode.andThen
            (\union ->
                Json.Decode.map2
                    (\packageIdentifier moduleIdentifier ->
                        Union packageIdentifier moduleIdentifier union
                    )
                    (Json.Decode.field "packageIdentifier" packageIdentifierDecoder)
                    (Json.Decode.field "moduleIdentifier" moduleIdentifierDecoder)
            )


aliasBlockDecoder : Json.Decode.Decoder Block
aliasBlockDecoder =
    Json.Decode.field "alias" Elm.Docs.aliasDecoder
        |> Json.Decode.andThen
            (\alias_ ->
                Json.Decode.map2
                    (\packageIdentifier moduleIdentifier ->
                        Alias packageIdentifier moduleIdentifier alias_
                    )
                    (Json.Decode.field "packageIdentifier" packageIdentifierDecoder)
                    (Json.Decode.field "moduleIdentifier" moduleIdentifierDecoder)
            )


valueBlockDecoder : Json.Decode.Decoder Block
valueBlockDecoder =
    Json.Decode.field "value" Elm.Docs.valueDecoder
        |> Json.Decode.andThen
            (\value ->
                Json.Decode.map2
                    (\packageIdentifier moduleIdentifier ->
                        Value packageIdentifier moduleIdentifier value
                    )
                    (Json.Decode.field "packageIdentifier" packageIdentifierDecoder)
                    (Json.Decode.field "moduleIdentifier" moduleIdentifierDecoder)
            )


binopBlockDecoder : Json.Decode.Decoder Block
binopBlockDecoder =
    Json.Decode.field "binop" Elm.Docs.binopDecoder
        |> Json.Decode.andThen
            (\binop ->
                Json.Decode.map2
                    (\packageIdentifier moduleIdentifier ->
                        Binop packageIdentifier moduleIdentifier binop
                    )
                    (Json.Decode.field "packageIdentifier" packageIdentifierDecoder)
                    (Json.Decode.field "moduleIdentifier" moduleIdentifierDecoder)
            )


encodeBlock : Block -> Json.Encode.Value
encodeBlock block =
    case block of
        Package _ _ ->
            Json.Encode.null

        Module packageIdentifier moduleIdentifier mod ->
            Json.Encode.object
                [ ( "packageIdentifier", encodePackageIdentifier packageIdentifier )
                , ( "moduleIdentifier", encodeModuleIdentifier moduleIdentifier )
                , ( "module", Json.Encode.null )
                ]

        Union packageIdentifier moduleIdentifier union ->
            Json.Encode.object
                [ ( "packageIdentifier", encodePackageIdentifier packageIdentifier )
                , ( "moduleIdentifier", encodeModuleIdentifier moduleIdentifier )
                , ( "union", encodeUnion union )
                ]

        Alias packageIdentifier moduleIdentifier alias_ ->
            Json.Encode.object
                [ ( "packageIdentifier", encodePackageIdentifier packageIdentifier )
                , ( "moduleIdentifier", encodeModuleIdentifier moduleIdentifier )
                , ( "alias", encodeAlias alias_ )
                ]

        Value packageIdentifier moduleIdentifier value ->
            Json.Encode.object
                [ ( "packageIdentifier", encodePackageIdentifier packageIdentifier )
                , ( "moduleIdentifier", encodeModuleIdentifier moduleIdentifier )
                , ( "value", encodeValue value )
                ]

        Binop packageIdentifier moduleIdentifier binop ->
            Json.Encode.object
                [ ( "packageIdentifier", encodePackageIdentifier packageIdentifier )
                , ( "moduleIdentifier", encodeModuleIdentifier moduleIdentifier )
                , ( "binop", encodeBinop binop )
                ]


encodeUnion : Elm.Docs.Union -> Json.Encode.Value
encodeUnion union =
    Json.Encode.object
        [ ( "name", Json.Encode.string union.name )
        , ( "comment", Json.Encode.string union.comment )
        , ( "args", Json.Encode.list Json.Encode.string union.args )
        , ( "cases", Json.Encode.list encodeTag union.tags )
        ]


encodeTag : ( String, List Elm.Type.Type ) -> Json.Encode.Value
encodeTag ( name, types ) =
    Json.Encode.list identity
        [ Json.Encode.string name
        , Json.Encode.list Json.Encode.string (List.map (elmTypeToString False) types)
        ]


encodeAlias : Elm.Docs.Alias -> Json.Encode.Value
encodeAlias alias_ =
    Json.Encode.object
        [ ( "name", Json.Encode.string alias_.name )
        , ( "comment", Json.Encode.string alias_.comment )
        , ( "args", Json.Encode.list Json.Encode.string alias_.args )
        , ( "type", Json.Encode.string (elmTypeToString False alias_.tipe) )
        ]


encodeValue : Elm.Docs.Value -> Json.Encode.Value
encodeValue value =
    Json.Encode.object
        [ ( "name", Json.Encode.string value.name )
        , ( "comment", Json.Encode.string value.comment )
        , ( "type", Json.Encode.string (elmTypeToString False value.tipe) )
        ]


encodeBinop : Elm.Docs.Binop -> Json.Encode.Value
encodeBinop binop =
    Json.Encode.object
        [ ( "name", Json.Encode.string binop.name )
        , ( "comment", Json.Encode.string binop.comment )
        , ( "type", Json.Encode.string (elmTypeToString False binop.tipe) )
        , ( "associativity", encodeAssociativity binop.associativity )
        , ( "precedence", Json.Encode.int binop.precedence )
        ]


encodeAssociativity : Elm.Docs.Associativity -> Json.Encode.Value
encodeAssociativity associativity =
    Json.Encode.string <|
        case associativity of
            Elm.Docs.Left ->
                "left"

            Elm.Docs.None ->
                "non"

            Elm.Docs.Right ->
                "right"


type alias PackageIdentifier =
    { name : Elm.Package.Name
    , version : Elm.Version.Version
    }


encodePackageIdentifier : PackageIdentifier -> Json.Encode.Value
encodePackageIdentifier packageIdentifier =
    Json.Encode.object
        [ ( "name", Elm.Package.encode packageIdentifier.name )
        , ( "version", Elm.Version.encode packageIdentifier.version )
        ]


packageIdentifierDecoder : Json.Decode.Decoder PackageIdentifier
packageIdentifierDecoder =
    Json.Decode.map2 PackageIdentifier
        (Json.Decode.field "name" Elm.Package.decoder)
        (Json.Decode.field "version" Elm.Version.decoder)


type alias ModuleIdentifier =
    { name : String }


encodeModuleIdentifier : ModuleIdentifier -> Json.Encode.Value
encodeModuleIdentifier moduleIdentifier =
    Json.Encode.object
        [ ( "name", Json.Encode.string moduleIdentifier.name )
        ]


moduleIdentifierDecoder : Json.Decode.Decoder ModuleIdentifier
moduleIdentifierDecoder =
    Json.Decode.map ModuleIdentifier
        (Json.Decode.field "name" Json.Decode.string)


elmTypeToString : Bool -> Elm.Type.Type -> String
elmTypeToString nested tipe =
    case tipe of
        Elm.Type.Var name ->
            name

        Elm.Type.Lambda ((Elm.Type.Lambda fromFrom fromTo) as fromLamda) to ->
            "(" ++ elmTypeToString False fromLamda ++ ") -> " ++ elmTypeToString False to

        Elm.Type.Lambda from to ->
            elmTypeToString False from ++ " -> " ++ elmTypeToString False to

        Elm.Type.Tuple [] ->
            "()"

        Elm.Type.Tuple types ->
            "( " ++ (types |> List.map (elmTypeToString False) |> String.join ", ") ++ " )"

        Elm.Type.Type name types ->
            name
                :: List.map (elmTypeToString True) types
                |> String.join " "
                |> (\typeString ->
                        if nested && not (List.isEmpty types) then
                            "(" ++ typeString ++ ")"

                        else
                            typeString
                   )

        Elm.Type.Record fields (Just extensible) ->
            "{ " ++ extensible ++ " | " ++ recordFieldsToString fields ++ " }"

        Elm.Type.Record fields Nothing ->
            "{ " ++ recordFieldsToString fields ++ " }"


recordFieldsToString : List ( String, Elm.Type.Type ) -> String
recordFieldsToString fields =
    fields
        |> List.map (\( name, tipe ) -> name ++ " : " ++ elmTypeToString False tipe)
        |> String.join ", "
