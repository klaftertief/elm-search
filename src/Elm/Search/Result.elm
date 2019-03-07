module Elm.Search.Result exposing
    ( Block(..)
    , ModuleIdentifier
    , PackageIdentifier
    , encodeBlock
    )

import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type
import Elm.Version
import Json.Encode


type Block
    = Package PackageIdentifier Elm.Project.PackageInfo
    | Module PackageIdentifier ModuleIdentifier Elm.Docs.Module
    | Union PackageIdentifier ModuleIdentifier Elm.Docs.Union
    | Alias PackageIdentifier ModuleIdentifier Elm.Docs.Alias
    | Value PackageIdentifier ModuleIdentifier Elm.Docs.Value
    | Binop PackageIdentifier ModuleIdentifier Elm.Docs.Binop


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
        ]


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


type alias ModuleIdentifier =
    { name : String }


encodeModuleIdentifier : ModuleIdentifier -> Json.Encode.Value
encodeModuleIdentifier moduleIdentifier =
    Json.Encode.object
        [ ( "name", Json.Encode.string moduleIdentifier.name )
        ]


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
                        if nested then
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
