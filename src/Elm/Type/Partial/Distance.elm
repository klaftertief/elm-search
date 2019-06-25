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

        -- ( Partial.Type typeName typeArgs, Type.Var varName ) ->
        --     Distance.varTypeDistance varName ( typeName, typeArgs )
        -- ( Partial.TypeQual typeQual typeName typeArgs, Type.Var varName ) ->
        --     Distance.varTypeDistance varName ( typeQual ++ "." ++ typeName, typeArgs )
        -- ( Partial.Type fromName fromArgs, Type.Type toName toArgs ) ->
        --     distanceApply ( canonicalN, argsN ) ( canonicalH, argsH )
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
