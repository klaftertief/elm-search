module Search.Chunk exposing (..)

import String
import Docs.Entry as Entry exposing (Entry)
import Docs.Type as Type exposing (Type)
import Docs.Package as Package exposing (Package)
import Docs.Version as Version exposing (Version)


type alias Chunk =
    { context : Context
    , tipe : Type
    , tipeNormalized : Type
    , docs : Maybe String
    , elmVersion : Maybe Version
    }


type alias Context =
    { userName : String
    , packageName : String
    , packageVersion : Version
    , moduleName : String
    , name : String
    }


packageChunks : Package -> List Chunk
packageChunks package =
    package.modules
        |> List.concatMap
            (\{ name, elmVersion, entries } ->
                List.map ((,,) name elmVersion) entries
            )
        |> List.map
            (\( name, maybeVersion, entry ) ->
                toChunk package name maybeVersion entry
            )


toChunk : Package -> String -> Maybe Version -> Entry -> Chunk
toChunk package moduleName elmVersion { name, docs, tipe } =
    { context = Context package.user package.name package.version moduleName name
    , tipe = tipe
    , tipeNormalized = Type.normalize tipe
    , docs = List.head (docs |> String.trim |> String.split "\n\n" |> List.filter (not << String.isEmpty))
    , elmVersion = elmVersion
    }


identifierHome : Context -> String
identifierHome { userName, packageName, packageVersion } =
    [ userName, packageName, Version.vsnToString packageVersion ]
        |> String.join "/"


rootUrl : String
rootUrl =
    "http://package.elm-lang.org"


pathToPackage : Context -> String
pathToPackage { userName, packageName, packageVersion } =
    [ rootUrl, "packages", userName, packageName, Version.vsnToString packageVersion ]
        |> String.join "/"


pathToModule : Context -> String
pathToModule ({ moduleName } as context) =
    [ pathToPackage context
    , String.map dotToDash moduleName
    ]
        |> String.join "/"


pathToValue : Context -> String
pathToValue ({ name } as context) =
    [ pathToModule context, name ]
        |> String.join "#"


dotToDash : Char -> Char
dotToDash char =
    if char == '.' then
        '-'
    else
        char
