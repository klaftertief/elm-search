port module Docs.Package.Cache exposing (check, onMissing, put)

import Docs.Package as Package


check : String -> Package.Metadata -> Cmd msg
check moduleName package =
    lookup { moduleName = moduleName, metadata = package }


port lookup : { moduleName : String, metadata : Package.Metadata } -> Cmd msg


port put : { moduleName : String, code : String } -> Cmd msg


port onMissing : (Package.Metadata -> msg) -> Sub msg
