module Elm.Search exposing
    ( valuesByName
    , valuesByType
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Search.Index as Index exposing (Index)
import Elm.Search.Query as Query exposing (Query)
import Elm.Search.Result as Result exposing (Block)
import Elm.Search.Score as Score exposing (Score)
import Elm.Type exposing (Type)
import Elm.Type.Distance


search : List Query -> Index -> List ( Float, Block )
search queries index =
    let
        blocks =
            indexBlocks index
    in
    List.map
        (\block -> ( bestBlockScore queries block, block ))
        blocks


bestBlockScore : List Query -> Block -> Float
bestBlockScore queries block =
    List.map (\query -> blockScore query block) queries
        |> List.minimum
        |> Maybe.withDefault (1 / 0)


blockScore : Query -> Block -> Float
blockScore query block =
    1


indexBlocks : Index -> List Block
indexBlocks index =
    []


valuesByName : String -> Index -> Dict String Elm.Docs.Value
valuesByName queryString =
    Index.allValues >> Dict.filter (\_ { name } -> String.contains queryString name)


valuesByType : Type -> Index -> Dict String Elm.Docs.Value
valuesByType queryType =
    Index.allValues >> Dict.filter (\_ { tipe } -> Elm.Type.Distance.distance queryType tipe < 0.2)
