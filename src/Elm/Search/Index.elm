module Elm.Search.Index exposing
    ( Index, empty
    , addPackage
    , allPackages, getPackage
    , ExposedIdentifier(..), ModuleIdentifier(..), PackageIdentifier(..), allBinops, allValues, findValuesByName, findValuesByType
    )

{-| Search Index

@docs Index, empty

@docs addPackage

@docs allPackages, getPackage

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
    { user : String
    , packageName : String
    , version : String
    , info : Elm.Project.PackageInfo
    }


type alias ModuleBlock =
    { user : String
    , packageName : String
    , version : String
    , moduleName : String
    , info : Elm.Docs.Module
    }


type alias ExposedBlock a =
    { user : String
    , packageName : String
    , version : String
    , moduleName : String
    , exposedName : String
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
removePackage (PackageIdentifier packageId) (Index index) =
    Index
        { packages = Dict.remove packageId index.packages
        , modules = Dict.filter (\moduleId _ -> String.startsWith moduleId packageId) index.modules
        , unions = Dict.filter (\exposedId _ -> String.startsWith exposedId packageId) index.unions
        , aliases = Dict.filter (\exposedId _ -> String.startsWith exposedId packageId) index.aliases
        , values = Dict.filter (\exposedId _ -> String.startsWith exposedId packageId) index.values
        , binops = Dict.filter (\exposedId _ -> String.startsWith exposedId packageId) index.binops
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
                    { user = user
                    , packageName = packageName
                    , version = version
                    , info = package.info
                    }

                moduleId mod =
                    packageId ++ "/" ++ mod.name

                moduleBlock mod =
                    { user = user
                    , packageName = packageName
                    , version = version
                    , moduleName = mod.name
                    , info = mod
                    }

                exposedId modId { name } =
                    modId ++ "/" ++ name

                exposedBlock mod info =
                    { user = user
                    , packageName = packageName
                    , version = version
                    , moduleName = mod.name
                    , exposedName = info.name
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


allPackages : Index -> Dict String PackageBlock
allPackages (Index index) =
    index.packages


getPackage : String -> Index -> Maybe PackageBlock
getPackage identifier =
    -- allPackages >> Dict.get (packageIdentifierToString identifier)
    allPackages >> Dict.get identifier


allModules : Index -> Dict String ModuleBlock
allModules (Index index) =
    index.modules


getModule : ModuleIdentifier -> Index -> Maybe ModuleBlock
getModule identifier =
    allModules >> Dict.get (moduleIdentifierToString identifier)


getUnion : ExposedIdentifier -> Index -> Maybe UnionBlock
getUnion identifier =
    allUnions >> Dict.get (exposedIdentifierToString identifier)


allUnions : Index -> Dict String UnionBlock
allUnions (Index index) =
    index.unions


getAlias : ExposedIdentifier -> Index -> Maybe AliasBlock
getAlias identifier =
    allAlias >> Dict.get (exposedIdentifierToString identifier)


allAlias : Index -> Dict String AliasBlock
allAlias (Index index) =
    index.aliases


getValue : ExposedIdentifier -> Index -> Maybe ValueBlock
getValue identifier =
    allValues >> Dict.get (exposedIdentifierToString identifier)


allValues : Index -> Dict String ValueBlock
allValues (Index index) =
    index.values


findValuesByName : String -> Index -> Dict String ValueBlock
findValuesByName queryString =
    allValues >> Dict.filter (\_ { info } -> String.contains queryString info.name)


findValuesByType : Type -> Index -> Dict String ValueBlock
findValuesByType queryType =
    allValues >> Dict.filter (\_ { info } -> TypeDistance.distance queryType info.tipe < 0.2)


getBinop : ExposedIdentifier -> Index -> Maybe BinopBlock
getBinop identifier =
    allBinops >> Dict.get (exposedIdentifierToString identifier)


allBinops : Index -> Dict String BinopBlock
allBinops (Index index) =
    index.binops


type Block
    = Package PackageIdentifier Elm.Project.PackageInfo
    | Module ModuleIdentifier Elm.Docs.Module
    | Union ExposedIdentifier Elm.Docs.Union
    | Alias ExposedIdentifier Elm.Docs.Alias
    | Value ExposedIdentifier Elm.Docs.Value
    | Binop ExposedIdentifier Elm.Docs.Binop


type Score
    = Score


type Query
    = Query


scoreBlockQuery : Query -> Block -> Score
scoreBlockQuery query block =
    Score


scoreBlockQueries : List Query -> Block -> ( Block, List ( Query, Score ) )
scoreBlockQueries queries block =
    ( block
    , List.map
        (\query -> ( query, scoreBlockQuery query block ))
        queries
    )



-- IDENTIFIER


type PackageIdentifier
    = PackageIdentifier String


packageIdentifierToString : PackageIdentifier -> String
packageIdentifierToString (PackageIdentifier id) =
    id


type ModuleIdentifier
    = ModuleIdentifier PackageIdentifier String


moduleIdentifierToString : ModuleIdentifier -> String
moduleIdentifierToString (ModuleIdentifier packageId moduleId) =
    packageIdentifierToString packageId ++ "/" ++ moduleId


type ExposedIdentifier
    = ExposedIdentifier ModuleIdentifier String


exposedIdentifierToString : ExposedIdentifier -> String
exposedIdentifierToString (ExposedIdentifier moduleId exposedId) =
    moduleIdentifierToString moduleId ++ "/" ++ exposedId
