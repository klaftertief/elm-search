module Elm.Search.Index exposing
    ( Index, empty, addPackage
    , allPackages, getPackage
    , allModules, getModule
    , allUnions, getUnion
    , allAliases, getAlias
    , allValues, getValue
    , allBinops, getBinop
    , PackageIdentifier, ModuleIdentifier, ExposedIdentifier
    )

{-| Search Index

@docs Index, empty, addPackage

@docs allPackages, getPackage
@docs allModules, getModule
@docs allUnions, getUnion
@docs allAliases, getAlias
@docs allValues, getValue
@docs allBinops, getBinop

@docs PackageIdentifier, ModuleIdentifier, ExposedIdentifier

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Search.Query as Query exposing (Query)
import Elm.Type as Type exposing (Type)
import Elm.Type.Distance as TypeDistance
import Elm.Version


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
