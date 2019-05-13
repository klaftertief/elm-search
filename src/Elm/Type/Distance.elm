module Elm.Type.Distance exposing
    ( distance
    , distanceList
    , varDistance
    , varTypeDistance
    )

import Dict exposing (Dict)
import Elm.Type as Type exposing (Type(..))
import List.Extra


distance : Type -> Type -> Float
distance from to =
    case ( from, to ) of
        ( Lambda from1 to1, Lambda from2 to2 ) ->
            (distance from1 from2 + distance to1 to2) / 2

        ( Var fromName, Var toName ) ->
            varDistance fromName toName

        ( Var varName, Type "Maybe.Maybe" [ Var wrappedVarName ] ) ->
            varDistance varName wrappedVarName

        ( Type "Maybe.Maybe" [ Var wrappedVarName ], Var varName ) ->
            varDistance varName wrappedVarName

        ( Var varName, Type "Result.Result" [ Var wrappedVarName ] ) ->
            varDistance varName wrappedVarName

        ( Type "Result.Result" [ Var wrappedVarName ], Var varName ) ->
            varDistance varName wrappedVarName

        ( Var varName, Type typeName typeArgs ) ->
            varTypeDistance varName ( typeName, typeArgs )

        ( Type typeName typeArgs, Var varName ) ->
            varTypeDistance varName ( typeName, typeArgs )

        ( Type canonicalN argsN, Type canonicalH argsH ) ->
            distanceApply ( canonicalN, argsN ) ( canonicalH, argsH )

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
    in
    if needleLength /= List.length hay then
        maxPenalty

    else
        List.Extra.permutations needle
            |> List.map
                (\curr ->
                    List.map2 distance curr hay
                        |> List.sum
                        |> (\a -> (/) a (toFloat needleLength))
                )
            |> List.minimum
            |> Maybe.withDefault maxPenalty


varDistance : String -> String -> Float
varDistance needle hay =
    if needle == hay then
        noPenalty

    else
        maxPenalty


varTypeDistance : String -> ( String, List Type ) -> Float
varTypeDistance varName ( typeName, typeArgs ) =
    case Dict.get varName reservedVars of
        Just typeList ->
            typeList
                |> List.map (distance (Type typeName typeArgs))
                |> List.minimum
                |> Maybe.withDefault maxPenalty

        Nothing ->
            maxPenalty


reservedVars : Dict String (List Type)
reservedVars =
    Dict.empty
        |> Dict.insert "number"
            [ Type "Basics.Float" []
            , Type "Basics.Int" []
            ]
        |> Dict.insert "comparable"
            [ Type "Basics.Float" []
            , Type "Basics.Int" []
            , Type "Basics.Char" []
            , Type "Basics.String" []
            , Tuple [ Var "comparable" ]
            , Tuple [ Var "comparable", Var "comparable" ]
            , Tuple [ Var "comparable", Var "comparable", Var "comparable" ]
            ]
        |> Dict.insert "appendable"
            [ Type "Basics.String" []
            , Type "List.List" [ Var "a" ]
            ]


distanceCanonical : String -> String -> Float
distanceCanonical needle hay =
    if needle == hay then
        noPenalty

    else if String.endsWith needle hay then
        noPenalty

    else if String.endsWith hay needle then
        noPenalty

    else if String.contains needle hay then
        mediumPenalty

    else
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
