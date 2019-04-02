module Elm.Search.Index exposing
    ( Index, empty
    , addPackage
    , allPackages, getPackage
    , ExposedIdentifier(..), ModuleIdentifier(..), PackageIdentifier(..), allValues
    )

{-| Search Index

@docs Index, empty

@docs addPackage

@docs allPackages, getPackage

-}

import AssocList as Dict exposing (Dict)
import Elm.Docs
import Elm.Package
import Elm.Project
import Elm.Version


type Index
    = Index
        { packages : Dict PackageIdentifier Elm.Project.PackageInfo
        , modules : Dict ModuleIdentifier Elm.Docs.Module
        , unions : Dict ExposedIdentifier Elm.Docs.Union
        , aliases : Dict ExposedIdentifier Elm.Docs.Alias
        , values : Dict ExposedIdentifier Elm.Docs.Value
        , binops : Dict ExposedIdentifier Elm.Docs.Binop
        }


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
    Index
        { packages = Dict.remove packageId index.packages
        , modules = Dict.filter (\moduleId _ -> moduleIdentifierBelongsToPackage packageId moduleId) index.modules
        , unions = Dict.filter (\exposedId _ -> exposedIdentifierBelongsToPackage packageId exposedId) index.unions
        , aliases = Dict.filter (\exposedId _ -> exposedIdentifierBelongsToPackage packageId exposedId) index.aliases
        , values = Dict.filter (\exposedId _ -> exposedIdentifierBelongsToPackage packageId exposedId) index.values
        , binops = Dict.filter (\exposedId _ -> exposedIdentifierBelongsToPackage packageId exposedId) index.binops
        }


type alias PackageData =
    { info : Elm.Project.PackageInfo
    , readme : String
    , modules : List Elm.Docs.Module
    }


addPackage : PackageData -> Index -> Index
addPackage package (Index index) =
    let
        packageName =
            Elm.Package.toString package.info.name

        packageVersion =
            Elm.Version.toString package.info.version

        packageId =
            PackageIdentifier (packageName ++ "/" ++ packageVersion)

        moduleId mod =
            ModuleIdentifier packageId mod.name

        exposedId modId { name } =
            ExposedIdentifier modId name

        newModules =
            package.modules
                |> List.map (\mod -> ( moduleId mod, mod ))
                |> Dict.fromList

        newExposed toExposedList =
            package.modules
                |> List.concatMap (\mod -> List.map (Tuple.pair (moduleId mod)) (toExposedList mod))
                |> List.map (\( modId, e ) -> ( exposedId modId e, e ))
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
        { packages = Dict.insert packageId package.info index.packages
        , modules = Dict.union newModules index.modules
        , unions = Dict.union newUnions index.unions
        , aliases = Dict.union newAliases index.aliases
        , values = Dict.union newValues index.values
        , binops = Dict.union newBinops index.binops
        }


getPackage : String -> Index -> Maybe Elm.Project.PackageInfo
getPackage identifier =
    allPackages >> Dict.get (PackageIdentifier identifier)


allPackages : Index -> Dict PackageIdentifier Elm.Project.PackageInfo
allPackages (Index index) =
    index.packages


getModule : ModuleIdentifier -> Index -> Maybe Elm.Docs.Module
getModule identifier =
    allModules >> Dict.get identifier


allModules : Index -> Dict ModuleIdentifier Elm.Docs.Module
allModules (Index index) =
    index.modules


getUnion : ExposedIdentifier -> Index -> Maybe Elm.Docs.Union
getUnion identifier =
    allUnions >> Dict.get identifier


allUnions : Index -> Dict ExposedIdentifier Elm.Docs.Union
allUnions (Index index) =
    index.unions


getAlias : ExposedIdentifier -> Index -> Maybe Elm.Docs.Alias
getAlias identifier =
    allAlias >> Dict.get identifier


allAlias : Index -> Dict ExposedIdentifier Elm.Docs.Alias
allAlias (Index index) =
    index.aliases


getValue : ExposedIdentifier -> Index -> Maybe Elm.Docs.Value
getValue identifier =
    allValues >> Dict.get identifier


allValues : Index -> Dict ExposedIdentifier Elm.Docs.Value
allValues (Index index) =
    index.values


getBinop : ExposedIdentifier -> Index -> Maybe Elm.Docs.Binop
getBinop identifier =
    allBinops >> Dict.get identifier


allBinops : Index -> Dict ExposedIdentifier Elm.Docs.Binop
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


moduleIdentifierBelongsToPackage : PackageIdentifier -> ModuleIdentifier -> Bool
moduleIdentifierBelongsToPackage packageId (ModuleIdentifier modulePackageId _) =
    packageId == modulePackageId


moduleIdentifierToString : ModuleIdentifier -> String
moduleIdentifierToString (ModuleIdentifier packageId moduleId) =
    packageIdentifierToString packageId ++ "/" ++ moduleId


type ExposedIdentifier
    = ExposedIdentifier ModuleIdentifier String


exposedIdentifierBelongsToPackage : PackageIdentifier -> ExposedIdentifier -> Bool
exposedIdentifierBelongsToPackage packageId (ExposedIdentifier exposedModuleId _) =
    moduleIdentifierBelongsToPackage packageId exposedModuleId


exposedIdentifierToString : ExposedIdentifier -> String
exposedIdentifierToString (ExposedIdentifier moduleId exposedId) =
    moduleIdentifierToString moduleId ++ "/" ++ exposedId
