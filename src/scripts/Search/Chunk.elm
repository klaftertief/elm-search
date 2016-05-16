module Search.Chunk exposing (..)

--where

import Package.Module.Entry as Entry exposing (Entry)
import Package.Module.Name as Name exposing (Name)
import Package.Module.Type as Type exposing (Type)
import Package.Package as Package exposing (Package)


type alias Chunk =
    { name : String
    , docs : String
    , tipe : Type
    , tipeNormalized : Type
    , packageIdentifier : String
    , moduleName : Name
    }


packageToChunks : Package -> List Chunk
packageToChunks package =
    package.modules
        |> List.concatMap
            (\mod ->
                List.map ((,) mod.name) mod.entries
            )
        |> List.map
            (\( name, entry ) ->
                toChunk (Package.identifier package) name entry
            )


toChunk : String -> Name -> Entry -> Chunk
toChunk packageIdentifier moduleName { name, docs, tipe } =
    { name = name
    , docs = docs
    , tipe = tipe
    , tipeNormalized = Type.normalize tipe
    , packageIdentifier = packageIdentifier
    , moduleName = moduleName
    }
