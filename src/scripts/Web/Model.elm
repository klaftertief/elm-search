module Web.Model exposing (..)

-- where

import Http
import Package.Package as Package exposing (Package)


type Model
    = Loading
    | Failed Http.Error
    | Success Info


type alias Info =
    { packages : List Package
    , query : String
    }


type Msg
    = Fail Http.Error
    | Load (List Package)
    | Query String
