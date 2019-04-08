module Elm.Search.Index exposing
    ( Index, empty, addPackage
    , allPackages, getPackage, PackageBlock
    , allModules, getModule, ModuleBlock
    , allUnions, getUnion, UnionBlock
    , allAliases, getAlias, AliasBlock
    , allValues, getValue, ValueBlock
    , allBinops, getBinop, BinopBlock
    , PackageIdentifier, ModuleIdentifier, ExposedIdentifier
    , packageIdentifierDecoder, moduleIdentifierDecoder, exposedIdentifierDecoder
    , encodePackageIdentifier, encodeModuleIdentifier, encodeExposedIdentifier
    , Block, toBlocks, blockDecoder, encodeBlock
    )

{-| Search Index

@docs Index, empty, addPackage

@docs allPackages, getPackage, PackageBlock
@docs allModules, getModule, ModuleBlock
@docs allUnions, getUnion, UnionBlock
@docs allAliases, getAlias, AliasBlock
@docs allValues, getValue, ValueBlock
@docs allBinops, getBinop, BinopBlock

@docs PackageIdentifier, ModuleIdentifier, ExposedIdentifier
@docs packageIdentifierDecoder, moduleIdentifierDecoder, exposedIdentifierDecoder
@docs encodePackageIdentifier, encodeModuleIdentifier, encodeExposedIdentifier

@docs Block, toBlocks, blockDecoder, encodeBlock

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Search.Query as Query exposing (Query)
import Elm.Type as Type exposing (Type)
import Elm.Type.Distance as TypeDistance
import Elm.Version
import Json.Decode
import Json.Encode


type Index
    = Index
        { packages : Dict String PackageBlock
        , modules : Dict String ModuleBlock
        , unions : Dict String UnionBlock
        , aliases : Dict String AliasBlock
        , values : Dict String ValueBlock
        , binops : Dict String BinopBlock
        }


type alias PackageBlock =
    { identifier : PackageIdentifier
    , info : Elm.Project.PackageInfo
    }


type alias ModuleBlock =
    { identifier : ModuleIdentifier
    , info : Elm.Docs.Module
    }


type alias ExposedBlock a =
    { identifier : ExposedIdentifier
    , info : a
    }


type alias UnionBlock =
    ExposedBlock Elm.Docs.Union


type alias AliasBlock =
    ExposedBlock Elm.Docs.Alias


type alias ValueBlock =
    ExposedBlock Elm.Docs.Value


type alias BinopBlock =
    ExposedBlock Elm.Docs.Binop


empty : Index
empty =
    Index
        { packages = Dict.empty
        , modules = Dict.empty
        , unions = Dict.empty
        , aliases = Dict.empty
        , values = Dict.empty
        , binops = Dict.empty
        }


removePackage : PackageIdentifier -> Index -> Index
removePackage packageId (Index index) =
    let
        package =
            packageIdentifierToString packageId
    in
    Index
        { packages = Dict.remove package index.packages
        , modules = Dict.filter (\moduleId _ -> String.startsWith moduleId package) index.modules
        , unions = Dict.filter (\exposedId _ -> String.startsWith exposedId package) index.unions
        , aliases = Dict.filter (\exposedId _ -> String.startsWith exposedId package) index.aliases
        , values = Dict.filter (\exposedId _ -> String.startsWith exposedId package) index.values
        , binops = Dict.filter (\exposedId _ -> String.startsWith exposedId package) index.binops
        }


type alias PackageData =
    { info : Elm.Project.PackageInfo
    , readme : String
    , modules : List Elm.Docs.Module
    }


addPackage : PackageData -> Index -> Index
addPackage package (Index index) =
    case package.info.name |> Elm.Package.toString |> String.split "/" of
        [ user, packageName ] ->
            let
                version =
                    Elm.Version.toString package.info.version

                packageId =
                    user ++ "/" ++ packageName ++ "/" ++ version

                packageBlock =
                    { identifier =
                        PackageIdentifier
                            { user = user
                            , packageName = packageName
                            , version = version
                            }
                    , info = package.info
                    }

                moduleId mod =
                    packageId ++ "/" ++ mod.name

                moduleBlock mod =
                    { identifier =
                        ModuleIdentifier
                            { user = user
                            , packageName = packageName
                            , version = version
                            , moduleName = mod.name
                            }
                    , info = mod
                    }

                exposedId modId { name } =
                    modId ++ "/" ++ name

                exposedBlock mod info =
                    { identifier =
                        ExposedIdentifier
                            { user = user
                            , packageName = packageName
                            , version = version
                            , moduleName = mod.name
                            , exposedName = info.name
                            }
                    , info = info
                    }

                newModules =
                    package.modules
                        |> List.map (\mod -> ( moduleId mod, moduleBlock mod ))
                        |> Dict.fromList

                newExposed toExposedList =
                    package.modules
                        |> List.concatMap (\mod -> List.map (Tuple.pair mod) (toExposedList mod))
                        |> List.map (\( mod, e ) -> ( exposedId (moduleId mod) e, exposedBlock mod e ))
                        |> Dict.fromList

                newUnions =
                    newExposed .unions

                newAliases =
                    newExposed .aliases

                newValues =
                    newExposed .values

                newBinops =
                    newExposed .binops
            in
            Index
                { packages = Dict.insert packageId packageBlock index.packages
                , modules = Dict.union newModules index.modules
                , unions = Dict.union newUnions index.unions
                , aliases = Dict.union newAliases index.aliases
                , values = Dict.union newValues index.values
                , binops = Dict.union newBinops index.binops
                }

        _ ->
            Index index


allPackages : Index -> List PackageBlock
allPackages (Index index) =
    Dict.values index.packages


getPackage : PackageIdentifier -> Index -> Maybe PackageBlock
getPackage identifier (Index index) =
    Dict.get (packageIdentifierToString identifier) index.packages


allModules : Index -> List ModuleBlock
allModules (Index index) =
    Dict.values index.modules


getModule : ModuleIdentifier -> Index -> Maybe ModuleBlock
getModule identifier (Index index) =
    Dict.get (moduleIdentifierToString identifier) index.modules


allUnions : Index -> List UnionBlock
allUnions (Index index) =
    Dict.values index.unions


getUnion : ExposedIdentifier -> Index -> Maybe UnionBlock
getUnion identifier (Index index) =
    Dict.get (exposedIdentifierToString identifier) index.unions


allAliases : Index -> List AliasBlock
allAliases (Index index) =
    Dict.values index.aliases


getAlias : ExposedIdentifier -> Index -> Maybe AliasBlock
getAlias identifier (Index index) =
    Dict.get (exposedIdentifierToString identifier) index.aliases


allValues : Index -> List ValueBlock
allValues (Index index) =
    Dict.values index.values


getValue : ExposedIdentifier -> Index -> Maybe ValueBlock
getValue identifier (Index index) =
    Dict.get (exposedIdentifierToString identifier) index.values


allBinops : Index -> List BinopBlock
allBinops (Index index) =
    Dict.values index.binops


getBinop : ExposedIdentifier -> Index -> Maybe BinopBlock
getBinop identifier (Index index) =
    Dict.get (exposedIdentifierToString identifier) index.binops



-- IDENTIFIER


type PackageIdentifier
    = PackageIdentifier
        { user : String
        , packageName : String
        , version : String
        }


packageIdentifierToString : PackageIdentifier -> String
packageIdentifierToString (PackageIdentifier id) =
    String.join "/" [ id.user, id.packageName, id.version ]


encodePackageIdentifier : PackageIdentifier -> Json.Encode.Value
encodePackageIdentifier (PackageIdentifier id) =
    Json.Encode.object
        [ ( "user", Json.Encode.string id.user )
        , ( "packageName", Json.Encode.string id.packageName )
        , ( "version", Json.Encode.string id.version )
        ]


packageIdentifierDecoder : Json.Decode.Decoder PackageIdentifier
packageIdentifierDecoder =
    Json.Decode.map3
        (\user packageName version ->
            PackageIdentifier
                { user = user
                , packageName = packageName
                , version = version
                }
        )
        (Json.Decode.field "user" Json.Decode.string)
        (Json.Decode.field "packageName" Json.Decode.string)
        (Json.Decode.field "version" Json.Decode.string)


type ModuleIdentifier
    = ModuleIdentifier
        { user : String
        , packageName : String
        , version : String
        , moduleName : String
        }


moduleIdentifierToString : ModuleIdentifier -> String
moduleIdentifierToString (ModuleIdentifier id) =
    String.join "/" [ id.user, id.packageName, id.version, id.moduleName ]


encodeModuleIdentifier : ModuleIdentifier -> Json.Encode.Value
encodeModuleIdentifier (ModuleIdentifier id) =
    Json.Encode.object
        [ ( "user", Json.Encode.string id.user )
        , ( "packageName", Json.Encode.string id.packageName )
        , ( "version", Json.Encode.string id.version )
        , ( "moduleName", Json.Encode.string id.moduleName )
        ]


moduleIdentifierDecoder : Json.Decode.Decoder ModuleIdentifier
moduleIdentifierDecoder =
    Json.Decode.map4
        (\user packageName version moduleName ->
            ModuleIdentifier
                { user = user
                , packageName = packageName
                , version = version
                , moduleName = moduleName
                }
        )
        (Json.Decode.field "user" Json.Decode.string)
        (Json.Decode.field "packageName" Json.Decode.string)
        (Json.Decode.field "version" Json.Decode.string)
        (Json.Decode.field "moduleName" Json.Decode.string)


type ExposedIdentifier
    = ExposedIdentifier
        { user : String
        , packageName : String
        , version : String
        , moduleName : String
        , exposedName : String
        }


exposedIdentifierToString : ExposedIdentifier -> String
exposedIdentifierToString (ExposedIdentifier id) =
    String.join "/" [ id.user, id.packageName, id.version, id.moduleName, id.exposedName ]


encodeExposedIdentifier : ExposedIdentifier -> Json.Encode.Value
encodeExposedIdentifier (ExposedIdentifier id) =
    Json.Encode.object
        [ ( "user", Json.Encode.string id.user )
        , ( "packageName", Json.Encode.string id.packageName )
        , ( "version", Json.Encode.string id.version )
        , ( "moduleName", Json.Encode.string id.moduleName )
        , ( "exposedName", Json.Encode.string id.exposedName )
        ]


exposedIdentifierDecoder : Json.Decode.Decoder ExposedIdentifier
exposedIdentifierDecoder =
    Json.Decode.map5
        (\user packageName version moduleName exposedName ->
            ExposedIdentifier
                { user = user
                , packageName = packageName
                , version = version
                , moduleName = moduleName
                , exposedName = exposedName
                }
        )
        (Json.Decode.field "user" Json.Decode.string)
        (Json.Decode.field "packageName" Json.Decode.string)
        (Json.Decode.field "version" Json.Decode.string)
        (Json.Decode.field "moduleName" Json.Decode.string)
        (Json.Decode.field "exposedName" Json.Decode.string)



-- BLOCKS


type Block
    = Package PackageBlock
    | Module ModuleBlock
    | Union UnionBlock
    | Alias AliasBlock
    | Value ValueBlock
    | Binop BinopBlock


toBlocks : Index -> List Block
toBlocks index =
    List.concat
        [ index |> allPackages |> List.map Package
        , index |> allModules |> List.map Module
        , index |> allUnions |> List.map Union
        , index |> allAliases |> List.map Alias
        , index |> allValues |> List.map Value
        , index |> allBinops |> List.map Binop
        ]


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
            (\info ->
                Json.Decode.map
                    (\id ->
                        Union
                            { identifier = id
                            , info = info
                            }
                    )
                    (Json.Decode.field "identifier" exposedIdentifierDecoder)
            )


aliasBlockDecoder : Json.Decode.Decoder Block
aliasBlockDecoder =
    Json.Decode.field "alias" Elm.Docs.aliasDecoder
        |> Json.Decode.andThen
            (\info ->
                Json.Decode.map
                    (\id ->
                        Alias
                            { identifier = id
                            , info = info
                            }
                    )
                    (Json.Decode.field "identifier" exposedIdentifierDecoder)
            )


valueBlockDecoder : Json.Decode.Decoder Block
valueBlockDecoder =
    Json.Decode.field "value" Elm.Docs.valueDecoder
        |> Json.Decode.andThen
            (\info ->
                Json.Decode.map
                    (\id ->
                        Value
                            { identifier = id
                            , info = info
                            }
                    )
                    (Json.Decode.field "identifier" exposedIdentifierDecoder)
            )


binopBlockDecoder : Json.Decode.Decoder Block
binopBlockDecoder =
    Json.Decode.field "binop" Elm.Docs.binopDecoder
        |> Json.Decode.andThen
            (\info ->
                Json.Decode.map
                    (\id ->
                        Binop
                            { identifier = id
                            , info = info
                            }
                    )
                    (Json.Decode.field "identifier" exposedIdentifierDecoder)
            )


encodeBlock : Block -> Json.Encode.Value
encodeBlock block =
    case block of
        Package package ->
            Json.Encode.object
                [ ( "identifier", encodePackageIdentifier package.identifier )
                , ( "package", Json.Encode.null )
                ]

        Module module_ ->
            Json.Encode.object
                [ ( "identifier", encodeModuleIdentifier module_.identifier )
                , ( "module", Json.Encode.null )
                ]

        Union union ->
            Json.Encode.object
                [ ( "identifier", encodeExposedIdentifier union.identifier )
                , ( "union", encodeUnion union.info )
                ]

        Alias alias_ ->
            Json.Encode.object
                [ ( "identifier", encodeExposedIdentifier alias_.identifier )
                , ( "alias", encodeAlias alias_.info )
                ]

        Value value ->
            Json.Encode.object
                [ ( "identifier", encodeExposedIdentifier value.identifier )
                , ( "alias", encodeValue value.info )
                ]

        Binop binop ->
            Json.Encode.object
                [ ( "identifier", encodeExposedIdentifier binop.identifier )
                , ( "binop", encodeBinop binop.info )
                ]


encodeUnion : Elm.Docs.Union -> Json.Encode.Value
encodeUnion union =
    Json.Encode.object
        [ ( "name", Json.Encode.string union.name )
        , ( "comment", Json.Encode.string union.comment )
        , ( "args", Json.Encode.list Json.Encode.string union.args )
        , ( "cases", Json.Encode.list encodeTag union.tags )
        ]


encodeTag : ( String, List Type ) -> Json.Encode.Value
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


elmTypeToString : Bool -> Type -> String
elmTypeToString nested tipe =
    case tipe of
        Type.Var name ->
            name

        Type.Lambda ((Type.Lambda fromFrom fromTo) as fromLamda) to ->
            "(" ++ elmTypeToString False fromLamda ++ ") -> " ++ elmTypeToString False to

        Type.Lambda from to ->
            elmTypeToString False from ++ " -> " ++ elmTypeToString False to

        Type.Tuple [] ->
            "()"

        Type.Tuple types ->
            "( " ++ (types |> List.map (elmTypeToString False) |> String.join ", ") ++ " )"

        Type.Type name types ->
            name
                :: List.map (elmTypeToString True) types
                |> String.join " "
                |> (\typeString ->
                        if nested && not (List.isEmpty types) then
                            "(" ++ typeString ++ ")"

                        else
                            typeString
                   )

        Type.Record fields (Just extensible) ->
            "{ " ++ extensible ++ " | " ++ recordFieldsToString fields ++ " }"

        Type.Record fields Nothing ->
            "{ " ++ recordFieldsToString fields ++ " }"


recordFieldsToString : List ( String, Type ) -> String
recordFieldsToString fields =
    fields
        |> List.map (\( name, tipe ) -> name ++ " : " ++ elmTypeToString False tipe)
        |> String.join ", "


elmTypeToText : Bool -> Type -> String
elmTypeToText nested tipe =
    case tipe of
        Type.Var name ->
            name
                |> String.split "."
                |> List.reverse
                |> List.head
                |> Maybe.withDefault name

        Type.Lambda ((Type.Lambda fromFrom fromTo) as fromLamda) to ->
            "(" ++ elmTypeToText False fromLamda ++ ") -> " ++ elmTypeToText False to

        Type.Lambda from to ->
            elmTypeToText False from ++ " -> " ++ elmTypeToText False to

        Type.Tuple [] ->
            "()"

        Type.Tuple types ->
            "( " ++ (types |> List.map (elmTypeToText False) |> String.join ", ") ++ " )"

        Type.Type name types ->
            (name
                |> String.split "."
                |> List.reverse
                |> List.head
                |> Maybe.withDefault name
            )
                :: List.map (elmTypeToText True) types
                |> String.join " "
                |> (\typeString ->
                        if nested && not (List.isEmpty types) then
                            "(" ++ typeString ++ ")"

                        else
                            typeString
                   )

        Type.Record fields (Just extensible) ->
            "{ " ++ extensible ++ " | " ++ recordFieldsToString fields ++ " }"

        Type.Record fields Nothing ->
            "{ " ++ recordFieldsToString fields ++ " }"
