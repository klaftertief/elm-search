module Elm.Search.Result exposing (Result(..))

import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Version


type Result
    = Package PackageIdentifier Elm.Project.PackageInfo
    | Module PackageIdentifier ModuleIdentifier Elm.Docs.Module
    | Union PackageIdentifier ModuleIdentifier Elm.Docs.Union
    | Alias PackageIdentifier ModuleIdentifier Elm.Docs.Alias
    | Value PackageIdentifier ModuleIdentifier Elm.Docs.Value
    | Binop PackageIdentifier ModuleIdentifier Elm.Docs.Binop


type alias PackageIdentifier =
    { name : Elm.Package.Name
    , version : Elm.Version.Version
    }


type alias ModuleIdentifier =
    { name : Elm.Module.Name }
