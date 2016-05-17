module Search.Chunk exposing (..)

--where

import String
import Package.Module.Entry as Entry exposing (Entry)
import Package.Module.Name as Name exposing (Name)
import Package.Module.Type as Type exposing (Type)
import Package.Package as Package exposing (Package)


type alias Chunk =
    { name : String
    , tipe : Type
    , tipeNormalized : Type
    , docs : Maybe String
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
    , tipe = tipe
    , tipeNormalized = Type.normalize tipe
    , docs = List.head (docs |> String.trim |> String.split "\n\n" |> List.filter (not << String.isEmpty))
    , packageIdentifier = packageIdentifier
    , moduleName = moduleName
    }
