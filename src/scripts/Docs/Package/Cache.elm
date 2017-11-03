port module Docs.Package.Cache exposing (check, onMissing, put)

import Docs.Package as Package


check : String -> Package.Partial -> Cmd msg
check moduleName package =
    lookup { moduleName = moduleName, identifier = package }


port lookup : { moduleName : String, identifier : Package.Partial } -> Cmd msg


port put : { moduleName : String, code : String } -> Cmd msg


port onMissing : (Package.Partial -> msg) -> Sub msg
