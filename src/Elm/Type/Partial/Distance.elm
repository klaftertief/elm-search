module Elm.Type.Partial.Distance exposing (distance)

import Dict exposing (Dict)
import Elm.Type as Type exposing (Type)
import Elm.Type.Distance as Distance
import Elm.Type.Partial as Partial exposing (Partial)
import List.Extra


distance : Partial -> Type -> Float
distance from to =
    case ( from, to ) of
        ( Partial.LambdaFrom partialFrom, Type.Lambda lambdaFrom _ ) ->
            Distance.distance partialFrom lambdaFrom

        ( Partial.LambdaTo partialTo, Type.Lambda _ lambdaTo ) ->
            Distance.distance partialTo lambdaTo

        ( Partial.VariableType _ variableTypeArgs, Type.Type _ typeArgs ) ->
            Distance.distanceList variableTypeArgs typeArgs

        _ ->
            maxPenalty


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
