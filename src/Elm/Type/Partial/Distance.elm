module Elm.Type.Partial.Distance exposing (distance)

import Dict exposing (Dict)
import Elm.Type as Type exposing (Type)
import Elm.Type.Distance as Distance
import Elm.Type.Partial as Partial
import List.Extra


distance : Partial.Type -> Type -> Float
distance from to =
    case ( from, to ) of
        -- PARTIAL
        ( Partial.LambdaFrom partialFrom, Type.Lambda lambdaFrom _ ) ->
            distance partialFrom lambdaFrom

        ( Partial.Lambda partialFrom (Partial.Unknown ""), Type.Lambda lambdaFrom _ ) ->
            distance partialFrom lambdaFrom

        ( Partial.LambdaTo partialTo, Type.Lambda _ lambdaTo ) ->
            distance partialTo lambdaTo

        ( Partial.VariableType _ variableTypeArgs, Type.Type _ typeArgs ) ->
            distanceList variableTypeArgs typeArgs

        -- CONCRETE
        ( Partial.Lambda from1 to1, Type.Lambda from2 to2 ) ->
            (distance from1 from2 + distance to1 to2) / 2

        ( Partial.Var fromName, Type.Var toName ) ->
            Distance.varDistance fromName toName

        ( Partial.Var varName, Type.Type "Maybe.Maybe" [ Type.Var wrappedVarName ] ) ->
            Distance.varDistance varName wrappedVarName

        ( Partial.Type "Maybe" [ Partial.Var wrappedVarName ], Type.Var varName ) ->
            Distance.varDistance varName wrappedVarName

        ( Partial.TypeQual "Maybe" "Maybe" [ Partial.Var wrappedVarName ], Type.Var varName ) ->
            Distance.varDistance varName wrappedVarName

        ( Partial.Var varName, Type.Type "Result.Result" [ Type.Var wrappedVarName ] ) ->
            Distance.varDistance varName wrappedVarName

        ( Partial.Type "Result" [ Partial.Var wrappedVarName ], Type.Var varName ) ->
            Distance.varDistance varName wrappedVarName

        ( Partial.TypeQual "Result" "Result" [ Partial.Var wrappedVarName ], Type.Var varName ) ->
            Distance.varDistance varName wrappedVarName

        ( Partial.Var varName, Type.Type typeName typeArgs ) ->
            Distance.varTypeDistance varName ( typeName, typeArgs )

        ( Partial.Type typeName typeArgs, Type.Var varName ) ->
            varTypeDistance varName ( typeName, typeArgs )

        ( Partial.TypeQual typeQual typeName typeArgs, Type.Var varName ) ->
            varTypeDistance varName ( typeQual ++ "." ++ typeName, typeArgs )

        ( Partial.Type fromName fromArgs, Type.Type toName toArgs ) ->
            distanceApply ( fromName, fromArgs ) ( toName, toArgs )

        ( Partial.Pair fromFirst fromSecond, Type.Tuple [ toFirst, toSecond ] ) ->
            distanceList [ fromFirst, fromSecond ] [ toFirst, toSecond ]

        ( Partial.Triple fromFirst fromSecond fromThird, Type.Tuple [ toFirst, toSecond, toThird ] ) ->
            distanceList [ fromFirst, fromSecond, fromThird ] [ toFirst, toSecond, toThird ]

        _ ->
            maxPenalty


distanceList : List Partial.Type -> List Type -> Float
distanceList from to =
    let
        fromLength =
            List.length from
    in
    if fromLength /= List.length to then
        maxPenalty

    else
        List.Extra.permutations from
            |> List.map
                (\currentFrom ->
                    List.map2 distance currentFrom to
                        |> List.sum
                        |> (\a -> (/) a (toFloat fromLength))
                )
            |> List.minimum
            |> Maybe.withDefault maxPenalty


distanceApply : ( String, List Partial.Type ) -> ( String, List Type ) -> Float
distanceApply ( canonicalN, argsN ) ( canonicalH, argsH ) =
    case ( argsN, argsH ) of
        ( [], [] ) ->
            distanceCanonical canonicalN canonicalH

        ( [], hd :: tl ) ->
            --distanceCanonical canonicalN canonicalH
            -- TODO: should we do this only for some specific types like `Maybe` and `Result`?
            -- TODO: check if this is a nice implementation (with regard to `min` and `+ lowPenalty`)
            min maxPenalty <|
                distance (Partial.Type canonicalN argsN)
                    (Maybe.withDefault hd (List.head (List.reverse tl)))
                    + lowPenalty

        _ ->
            (distanceCanonical canonicalN canonicalH + distanceList argsN argsH) / 2


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


varTypeDistance : String -> ( String, List Partial.Type ) -> Float
varTypeDistance varName ( typeName, typeArgs ) =
    case Dict.get varName Distance.internalTypeClasses of
        Just typeList ->
            typeList
                |> List.map (distance (Partial.Type typeName typeArgs))
                |> List.minimum
                |> Maybe.withDefault maxPenalty

        Nothing ->
            maxPenalty


internalTypeClasses : Dict String (List Partial.Type)
internalTypeClasses =
    Dict.empty
        |> Dict.insert "number"
            [ Partial.TypeQual "Basics" "Float" []
            , Partial.TypeQual "Basics" "Int" []
            ]
        |> Dict.insert "comparable"
            [ Partial.TypeQual "Basics" "Float" []
            , Partial.TypeQual "Basics" "Int" []
            , Partial.TypeQual "Basics" "Char" []
            , Partial.TypeQual "Basics" "String" []
            , Partial.Var "comparable"
            , Partial.Pair (Partial.Var "comparable") (Partial.Var "comparable")
            , Partial.Triple (Partial.Var "comparable") (Partial.Var "comparable") (Partial.Var "comparable")
            ]
        |> Dict.insert "appendable"
            [ Partial.TypeQual "Basics" "String" []
            , Partial.TypeQual "List" "List" [ Partial.Var "a" ]
            ]


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
