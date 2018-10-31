module Logo exposing (view, viewWithSize)

-- where

import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Svg msg
view =
    viewWithSize 128


viewWithSize : Int -> Svg msg
viewWithSize s =
    let
        size =
            toString s
    in
    svg
        [ width size
        , height size
        , viewBox "2 2 132 132"
        , Svg.Attributes.style "overflow: visible;"
        ]
        [ g
            [ stroke "#fff"
            , strokeWidth "2"
            , strokeLinejoin "round"

            --, transform "rotate(90 64 64)"
            ]
            parts
        ]


parts : List (Svg msg)
parts =
    [ largeBlueTriangle (Transformation (3 * quarterHypothenuse - 128) (3 * quarterHypothenuse) -90)
    , largeGreyTriangle (Transformation 0 0 0)
    , mediumBlueTriangle (Transformation quarterHypothenuse quarterHypothenuse -135)
    , smallOrangeTriangle (Transformation halfHypothenuse quarterHypothenuse -45)
    , smallOrangeTriangle (Transformation halfHypothenuse 0 45)
    , greenSquare (Transformation halfHypothenuse quarterHypothenuse -45)
    , greenDiamond (Transformation halfHypothenuse quarterHypothenuse 45) False
    ]


halfHypothenuse : Float
halfHypothenuse =
    128 / sqrt 2


quarterHypothenuse : Float
quarterHypothenuse =
    128 / 2 / sqrt 2


orange : String
orange =
    "#F0AD00"


blue : String
blue =
    "#60B5CC"


green : String
green =
    "#7FD13B"


grey : String
grey =
    "#5A6378"


type alias Transformation =
    { movementX : Float
    , movementY : Float
    , rotation : Float
    }


toTransform : Transformation -> String
toTransform { movementX, movementY, rotation } =
    "translate("
        ++ toString movementX
        ++ " "
        ++ toString movementY
        ++ ") rotate("
        ++ toString rotation
        ++ ")"


largeBlueTriangle : Transformation -> Svg msg
largeBlueTriangle transformation =
    polygon
        [ points "0,0 0,128 64,64"
        , fill blue
        , transform (toTransform transformation)
        ]
        []


largeGreyTriangle : Transformation -> Svg msg
largeGreyTriangle transformation =
    polygon
        [ points "0,0 0,128 64,64"
        , fill grey
        , transform (toTransform transformation)
        ]
        []


mediumBlueTriangle : Transformation -> Svg msg
mediumBlueTriangle transformation =
    polygon
        [ points "0,0 0,64 64,0"
        , fill blue
        , transform (toTransform transformation)
        ]
        []


smallOrangeTriangle : Transformation -> Svg msg
smallOrangeTriangle transformation =
    polygon
        [ points "0,0 0,64 32,32"
        , fill orange
        , transform (toTransform transformation)
        ]
        []


greenSquare : Transformation -> Svg msg
greenSquare transformation =
    polygon
        [ points "0,0 32,-32 64,0 32,32"
        , fill green
        , transform (toTransform transformation)
        ]
        []


greenDiamond : Transformation -> Bool -> Svg msg
greenDiamond transformation flip =
    polygon
        [ points "0,0 64,0 96,32 32,32"
        , fill green
        , transform <|
            toTransform transformation
                ++ (if \f b a -> f a b then
                        " scale(1 -1)"

                    else
                        ""
                   )
        ]
        []
