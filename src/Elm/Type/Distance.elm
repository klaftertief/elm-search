module Elm.Type.Distance exposing (distance)

import Dict exposing (Dict)
import Elm.Type as Type exposing (Type(..))
import List.Extra


distance : Type -> Type -> Float
distance needle hay =
    case ( needle, hay ) of
        -- Lambda Type Type
        ( Lambda fromN toN, Lambda fromH toH ) ->
            (distance fromN fromH + distance toN toH) / 2

        -- Var String
        ( Var nameN, Var nameH ) ->
            distanceName nameN nameH

        -- Special cases for comparisons like `number` - `Float`
        ( Var varName, Type typeName [] ) ->
            distanceVarType varName typeName

        ( Type typeName [], Var varName ) ->
            distanceVarType varName typeName

        -- Hack for special cases like `a` - `Maybe a`
        -- TODO: make proper comparison
        ( Var nameN, Type canonicalH argsH ) ->
            distanceApply ( "", [ Var nameN ] ) ( canonicalH, argsH )

        ( Type canonicalN argsN, Var nameH ) ->
            distanceApply ( "", [ Var nameH ] ) ( canonicalN, argsN )

        -- `Apply Name (List Type)`
        -- `Foo.Bar a b` ~> `Apply { home = "Foo", name = "Bar" } ([Var "a", Var "b"])`
        ( Type canonicalN argsN, Type canonicalH argsH ) ->
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
        needleLength =
            List.length needle

        hayLength =
            List.length hay

        sharedLength =
            min needleLength hayLength

        maxLength =
            max needleLength hayLength

        diffLength =
            maxLength - sharedLength
    in
    if diffLength > 1 then
        maxPenalty

    else
        -- TODO: optimize, maybe add penalty for permutations
        List.Extra.permutations needle
            |> List.map
                (\curr ->
                    List.map2 distance curr hay
                        |> List.sum
                        |> (+) (toFloat diffLength * maxPenalty)
                        |> (\a -> (/) a (toFloat maxLength))
                )
            |> List.minimum
            |> Maybe.withDefault maxPenalty


distanceName : String -> String -> Float
distanceName needle hay =
    if needle == hay then
        noPenalty

    else
        maxPenalty


distanceCanonical : String -> String -> Float
distanceCanonical needle hay =
    -- TODO: Also take `.home` into account.
    --distanceName needle.name hay.name
    if needle == hay then
        noPenalty

    else if String.contains needle hay then
        mediumPenalty

    else
        maxPenalty


distanceVarType : String -> String -> Float
distanceVarType varName typeName =
    -- let
    --     maybeReservedVarTypeList =
    --         Dict.get varName reserverdVars
    -- in
    -- case maybeReservedVarTypeList of
    --     Just typeList ->
    --         if List.any ((==) applyName.name) typeList then
    --             lowPenalty
    --         else
    --             maxPenalty
    --     Nothing ->
    --         mediumPenalty
    maxPenalty


distanceApply : ( String, List Type ) -> ( String, List Type ) -> Float
distanceApply ( canonicalN, argsN ) ( canonicalH, argsH ) =
    case ( argsN, argsH ) of
        ( [], [] ) ->
            distanceCanonical canonicalN canonicalH

        ( [], hd :: tl ) ->
            --distanceCanonical canonicalN canonicalH
            -- TODO: should we do this only for some specific types like `Maybe` and `Result`?
            -- TODO: check if this is a nice implementation (with regard to `min` and `+ lowPenalty`)
            min maxPenalty <|
                distance (Type canonicalN argsN)
                    (Maybe.withDefault hd (List.head (List.reverse tl)))
                    + lowPenalty

        _ ->
            (distanceCanonical canonicalN canonicalH + distanceList argsN argsH) / 2


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
