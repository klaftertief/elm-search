module Elm.Search exposing (search)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Search.Index as Index exposing (Block, Index)
import Elm.Search.Query as Query exposing (Query)
import Elm.Search.Score as Score exposing (Score)
import Elm.Type exposing (Type)
import Elm.Type.Distance


search : List Query -> Index -> List ( Float, Block )
search queries index =
    List.map
        (\block -> ( bestBlockScore queries block, block ))
        (Index.toBlocks index)


bestBlockScore : List Query -> Block -> Float
bestBlockScore queries block =
    List.map (\query -> blockScore query block) queries
        |> List.minimum
        |> Maybe.withDefault (1 / 0)


blockScore : Query -> Block -> Float
blockScore query block =
    1
