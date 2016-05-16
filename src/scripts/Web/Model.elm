module Web.Model exposing (..)

-- where

import Http
import Package.Package as Package exposing (Package)
import Search.Chunk as Chunk exposing (Chunk)


type Model
    = Loading
    | Failed Http.Error
    | Success Info


type alias Info =
    { chunks : List Chunk
    , query : String
    }


type Msg
    = Fail Http.Error
    | Load (List Package)
    | Query String
