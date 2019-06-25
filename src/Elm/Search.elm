module Elm.Search exposing (search)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Search.Index as Index exposing (Block, Index)
import Elm.Search.Query as Query exposing (Query)
import Elm.Search.Score as Score exposing (Score)
import Elm.Type exposing (Type)
import Elm.Type.Distance as TypeDistance
import Elm.Type.Partial.Distance as PartialTypeDistance


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
blockScore q b =
    case ( q, b ) of
        -- BY NAME
        ( Query.ByName name, Index.Package block ) ->
            if name == Index.packageIdentifierToString block.identifier then
                0

            else if String.contains name (Index.packageIdentifierToString block.identifier) then
                0.01

            else
                1

        ( Query.ByName name, Index.Module block ) ->
            if name == Index.moduleIdentifierToString block.identifier then
                0

            else if String.contains name (Index.moduleIdentifierToString block.identifier) then
                0.01

            else
                1

        ( Query.ByName name, Index.Union block ) ->
            if name == Index.exposedIdentifierToString block.identifier then
                0

            else if String.contains name (Index.exposedIdentifierToString block.identifier) then
                0.1

            else
                1

        ( Query.ByName name, Index.Alias block ) ->
            if name == Index.exposedIdentifierToString block.identifier then
                0

            else if String.contains name (Index.exposedIdentifierToString block.identifier) then
                0.1

            else
                1

        ( Query.ByName name, Index.Value block ) ->
            if name == Index.exposedIdentifierToString block.identifier then
                0

            else if name == block.info.name then
                0

            else if String.endsWith name (Index.exposedIdentifierToString block.identifier) then
                0.01

            else if String.contains name (Index.exposedIdentifierToString block.identifier) then
                0.1

            else
                1

        ( Query.ByName name, Index.Binop block ) ->
            if name == Index.exposedIdentifierToString block.identifier then
                0

            else if String.contains name (Index.exposedIdentifierToString block.identifier) then
                0.1

            else
                1

        -- BY TEXT
        ( Query.ByText text, Index.Package block ) ->
            if String.contains text (String.toLower block.info.summary) then
                0.1

            else
                1

        ( Query.ByText text, Index.Module block ) ->
            if String.contains text (String.toLower block.info.comment) then
                0.1

            else
                1

        ( Query.ByText text, Index.Union block ) ->
            if String.contains text block.info.comment then
                0.1

            else
                1

        ( Query.ByText text, Index.Alias block ) ->
            if String.contains text block.info.comment then
                0.1

            else
                1

        ( Query.ByText text, Index.Value block ) ->
            if String.contains text block.info.comment then
                0.1

            else
                1

        ( Query.ByText text, Index.Binop block ) ->
            if String.contains text block.info.comment then
                0.1

            else
                1

        -- BY TYPE
        ( Query.ByType tipe, Index.Package block ) ->
            1

        ( Query.ByType tipe, Index.Module block ) ->
            1

        ( Query.ByType tipe, Index.Union block ) ->
            block.info.tags
                |> List.map (\( name, types ) -> TypeDistance.distance tipe (Elm.Type.Type name types))
                |> List.minimum
                |> Maybe.withDefault 1

        ( Query.ByType tipe, Index.Alias block ) ->
            TypeDistance.distance tipe block.info.tipe

        ( Query.ByType tipe, Index.Value block ) ->
            TypeDistance.distance tipe block.info.tipe

        ( Query.ByType tipe, Index.Binop block ) ->
            TypeDistance.distance tipe block.info.tipe

        -- BY PARTIAL TYPE
        ( Query.ByPartialType tipe, Index.Package block ) ->
            1

        ( Query.ByPartialType tipe, Index.Module block ) ->
            1

        ( Query.ByPartialType tipe, Index.Union block ) ->
            block.info.tags
                |> List.map (\( name, types ) -> PartialTypeDistance.distance tipe (Elm.Type.Type name types))
                |> List.minimum
                |> Maybe.withDefault 1

        ( Query.ByPartialType tipe, Index.Alias block ) ->
            PartialTypeDistance.distance tipe block.info.tipe

        ( Query.ByPartialType tipe, Index.Value block ) ->
            PartialTypeDistance.distance tipe block.info.tipe

        ( Query.ByPartialType tipe, Index.Binop block ) ->
            PartialTypeDistance.distance tipe block.info.tipe
