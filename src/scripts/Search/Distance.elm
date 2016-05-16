module Search.Distance exposing (..)

-- where

import Char
import Dict exposing (Dict)
import String
import Package.Module.Entry as Entry exposing (Entry)
import Package.Module.Name as Name exposing (Name)
import Package.Module.Type as Type exposing (..)


name : String -> Entry -> Float
name query entry =
    if query == entry.name then
        noPenalty
    else if String.contains query entry.name then
        mediumPenalty * (1 - (toFloat (String.length query) / toFloat (String.length entry.name)))
    else
        maxPenalty


tipe : Type -> Entry -> Float
tipe query entry =
    distance (normalize query) (normalize entry.tipe)


distance : Type -> Type -> Float
distance needle hay =
    case ( needle, hay ) of
        {- Compare two functions `Function (List Type) Type`
           Functions get parsed like `a -> b` ~> `Function ([Var "a"]) (Var "b")`
           TODO: support three different comparisons
             - strict: length of arguments have to match
             - from beginning: concat args and result and compare the list
             - from end: concat args and result and compare the reversed list
           TODO: add some kind of mapping for vars in fuzzy calculations
        -}
        ( Function argsN resultN, Function argsH resultH ) ->
            let
                -- Handle special cases with singleton `Var` Type args
                argsDistance =
                    case ( argsN, argsH ) of
                        -- Compare `a -> r` and `b -> s`
                        ( [ Var n ], [ Var h ] ) ->
                            distanceName n h

                        -- Compare `a -> r` and `b -> c -> s`
                        -- This is the important special case.
                        ( [ Var n ], _ ) ->
                            mediumPenalty

                        -- The default case
                        _ ->
                            distanceList argsN argsH

                resultDistance =
                    distance resultN resultH
            in
                (argsDistance + resultDistance) / 2

        -- `Var String`
        -- `a` ~> `Var "a"`
        ( Var nameN, Var nameH ) ->
            distanceName nameN nameH

        -- Special cases for comparisons like `number` - `Float`
        ( Var nameN, Apply canonicalH _ ) ->
            distanceVarApply nameN canonicalH

        ( Apply canonicalN _, Var nameH ) ->
            distanceVarApply nameH canonicalN

        -- `Apply Name (List Type)`
        -- `Foo.Bar a b` ~> `Apply { home = "Foo", name = "Bar" } ([Var "a", Var "b"])`
        ( Apply canonicalN argsN, Apply canonicalH argsH ) ->
            distanceApply ( canonicalN, argsN ) ( canonicalH, argsH )

        -- Tuple (List Type)
        -- `(a,b)` ~> `Tuple ([Var "a",Var "b"])`
        ( Tuple argsN, Tuple argsH ) ->
            distanceList argsN argsH

        -- TODO: Record (List ( String, Type )) (Maybe String)
        {- The incomparable case
           TODO: Find and add special cases
        -}
        _ ->
            maxPenalty


distanceList : List Type -> List Type -> Float
distanceList needle hay =
    let
        needelLength =
            List.length needle

        hayLength =
            List.length hay

        sharedLength =
            min needelLength hayLength

        maxLength =
            max needelLength hayLength

        diffLength =
            maxLength - sharedLength
    in
        List.map2 distance needle hay
            |> List.sum
            |> (flip (+)) (toFloat diffLength * maxPenalty)
            |> (flip (/)) (toFloat maxLength)


distanceName : String -> String -> Float
distanceName needle hay =
    if needle == hay then
        noPenalty
    else
        maxPenalty


distanceCanonical : Name -> Name -> Float
distanceCanonical needle hay =
    -- TODO: Also take `.home` into account.
    --distanceName needle.name hay.name
    if needle.name == hay.name then
        noPenalty
    else if String.contains needle.name hay.name then
        mediumPenalty
    else
        maxPenalty


distanceVarApply : String -> Name -> Float
distanceVarApply varName applyName =
    let
        maybeReservedVarTypeList =
            Dict.get varName reserverdVars
    in
        case maybeReservedVarTypeList of
            Just typeList ->
                if List.any ((==) applyName.name) typeList then
                    lowPenalty
                else
                    maxPenalty

            Nothing ->
                mediumPenalty


distanceApply : ( Name, List Type ) -> ( Name, List Type ) -> Float
distanceApply ( canonicalN, argsN ) ( canonicalH, argsH ) =
    case ( argsN, argsH ) of
        ( [], [] ) ->
            distanceCanonical canonicalN canonicalH

        ( [], hd :: tl ) ->
            --distanceCanonical canonicalN canonicalH
            -- TODO: should we do this only for some specific types like `Maybe` and `Result`?
            -- TODO: check if this is a nice implementation (with regard to `min` and `+ lowPenalty`)
            min maxPenalty
                <| distance (Apply canonicalN argsN)
                    (Maybe.withDefault hd (List.head (List.reverse tl)))
                + lowPenalty

        _ ->
            (distanceCanonical canonicalN canonicalH + distanceList argsN argsH) / 2


reserverdVars : Dict String (List String)
reserverdVars =
    Dict.empty
        |> Dict.insert "number" [ "Float", "Int" ]
        |> Dict.insert "comparable" [ "Float", "Int", "Char", "String" ]
        |> Dict.insert "appendable" [ "String", "List" ]


type alias Mapping =
    Dict String String


defaultMapping : Mapping
defaultMapping =
    Dict.keys reserverdVars
        |> List.map (\v -> ( v, v ))
        |> Dict.fromList


nextMappingValue : Mapping -> String
nextMappingValue mapping =
    let
        base =
            (Dict.size mapping) - (Dict.size defaultMapping)

        code =
            (base % 26) + (Char.toCode 'a')

        string =
            String.fromChar (Char.fromCode code)

        times =
            (base // 26) + 1
    in
        String.repeat times string


updateMapping : Type -> Mapping -> Mapping
updateMapping tipe mapping =
    let
        updateMappingFor name =
            if Dict.member name mapping then
                mapping
            else
                Dict.insert name
                    (nextMappingValue mapping)
                    mapping
    in
        case tipe of
            Function args result ->
                List.foldl updateMapping mapping (List.append args [ result ])

            Var name ->
                updateMappingFor name

            Apply name args ->
                List.foldl updateMapping mapping args

            Tuple args ->
                List.foldl updateMapping mapping args

            Record fields ext ->
                List.foldl updateMapping mapping (List.map (\( _, t ) -> t) fields)


normalize : Type -> Type
normalize tipe =
    normalizeWithMapping (updateMapping tipe defaultMapping) tipe


normalizeWithMapping : Mapping -> Type -> Type
normalizeWithMapping mapping tipe =
    let
        normalize' =
            normalizeWithMapping mapping
    in
        case tipe of
            Function args result ->
                Function (List.map normalize' args)
                    (normalize' result)

            Var name ->
                let
                    name' =
                        case Dict.get name mapping of
                            Just n ->
                                n

                            Nothing ->
                                name
                in
                    Var name'

            Apply name args ->
                Apply name (List.map normalize' args)

            Tuple args ->
                Tuple (List.map normalize' args)

            Record fields ext ->
                Record (List.map (\( k, v ) -> ( k, normalize' v )) fields) ext


noPenalty : Float
noPenalty =
    0


lowPenalty : Float
lowPenalty =
    0.25


mediumPenalty : Float
mediumPenalty =
    0.5


highPenalty : Float
highPenalty =
    0.75


maxPenalty : Float
maxPenalty =
    1
