module Elm.Search.Index exposing
    ( Index, empty
    , addPackage
    , allPackages, getPackage
    , ExposedIdentifier(..), ModuleIdentifier(..), PackageIdentifier(..), allBinops, allValues
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
import Elm.Version


type Index
    = Index
        { packages : Dict String Elm.Project.PackageInfo
        , modules : Dict String Elm.Docs.Module
        , unions : Dict String Elm.Docs.Union
        , aliases : Dict String Elm.Docs.Alias
        , values : Dict String Elm.Docs.Value
        , binops : Dict String Elm.Docs.Binop
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
    let
        packageName =
            Elm.Package.toString package.info.name

        packageVersion =
            Elm.Version.toString package.info.version

        packageId =
            packageName ++ "/" ++ packageVersion

        moduleId mod =
            packageId ++ "/" ++ mod.name

        exposedId modId { name } =
            modId ++ "/" ++ name

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
    -- allPackages >> Dict.get (packageIdentifierToString identifier)
    allPackages >> Dict.get identifier


allPackages : Index -> Dict String Elm.Project.PackageInfo
allPackages (Index index) =
    index.packages


getModule : ModuleIdentifier -> Index -> Maybe Elm.Docs.Module
getModule identifier =
    allModules >> Dict.get (moduleIdentifierToString identifier)


allModules : Index -> Dict String Elm.Docs.Module
allModules (Index index) =
    index.modules


getUnion : ExposedIdentifier -> Index -> Maybe Elm.Docs.Union
getUnion identifier =
    allUnions >> Dict.get (exposedIdentifierToString identifier)


allUnions : Index -> Dict String Elm.Docs.Union
allUnions (Index index) =
    index.unions


getAlias : ExposedIdentifier -> Index -> Maybe Elm.Docs.Alias
getAlias identifier =
    allAlias >> Dict.get (exposedIdentifierToString identifier)


allAlias : Index -> Dict String Elm.Docs.Alias
allAlias (Index index) =
    index.aliases


getValue : ExposedIdentifier -> Index -> Maybe Elm.Docs.Value
getValue identifier =
    allValues >> Dict.get (exposedIdentifierToString identifier)


allValues : Index -> Dict String Elm.Docs.Value
allValues (Index index) =
    index.values


findValuesByName : String -> Index -> Dict String Elm.Docs.Value
findValuesByName queryString =
    allValues >> Dict.filter (\_ { name } -> String.contains queryString name)


getBinop : ExposedIdentifier -> Index -> Maybe Elm.Docs.Binop
getBinop identifier =
    allBinops >> Dict.get (exposedIdentifierToString identifier)


allBinops : Index -> Dict String Elm.Docs.Binop
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
