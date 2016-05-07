module Style.Search (css) where

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Style.Shared exposing (..)


rem' =
  Css.rem


textColor =
  "293c4b"


titleColor =
  "60b5cc"


titleHighlightColor =
  "ea157a"


stringColor =
  "99cc99"


css : Stylesheet
css =
  (stylesheet << namespace cssNamespace.name)
    [ everything
        [ boxSizing borderBox ]
    , html
        [ fontSize (px 16)
        ]
    , body
        [ fontSize (rem' 1)
        , lineHeight (pct 150)
        , fontFamilies
            [ "Source Sans Pro"
            , .value sansSerif
            ]
        , backgroundColor (rgb 235 235 235)
        , color (hex textColor)
        , margin zero
        ]
    , a [ color (hex titleColor) ]
    , code
        [ fontFamilies
            [ "Source Code Pro"
            , "consolas"
            , "inconsolata"
            , .value monospace
            ]
        ]
    , (.)
        SearchInput
        [ position fixed
        , top zero
        , left zero
        , right zero
        , padding (rem' 0.5)
        , backgroundColor (rgb 222 222 222)
        , borderBottom3 (px 1) solid (rgb 200 200 200)
        , children
            [ input
                [ display block
                , fontSize (rem' 1)
                , fontFamilies
                    [ "Source Code Pro"
                    , .value monospace
                    ]
                , width (pct 100)
                , border3 (px 1) solid (rgb 200 200 200)
                , borderRadius (rem' 0.125)
                , padding (rem' 0.25)
                ]
            , button
                [ position absolute
                , right (rem' 0.75)
                , top (rem' 0.5)
                , height (rem' 1.875)
                , boxSizing borderBox
                , backgroundColor transparent
                , border zero
                , fontSize (rem' 1.5)
                , lineHeight (pct 100)
                , color (hex titleHighlightColor)
                ]
            ]
        ]
    , (.)
        "App"
        [ padding2 (rem' 4.5) (rem' 1) ]
    , (.)
        "Result"
        [ marginBottom (rem' 1.5)
        , backgroundColor (rgb 255 255 255)
        , color (rgb 108 126 143)
          --, border3 (px 1) solid (rgb 45 45 45)
          --, border3 (px 1) solid (rgb 222 222 222)
        , borderRadius (rem' 0.125)
        , overflow hidden
        , property "box-shadow" "0 1px 2px rgba(0,0,0,0.2)"
        , descendants
            [ p [ margin zero ]
            , pre [ margin zero ]
            , a
                [ textDecoration none
                , hover
                    [ textDecoration underline
                    ]
                ]
            ]
        ]
    , (.)
        "Annotation"
        [ padding2 (rem' 0.75) (rem' 1)
        , backgroundColor (rgb 45 45 45)
        , color (rgb 204 204 204)
        , overflow scroll
        ]
    , (.)
        "Description"
        [ display block
        , padding2 (rem' 0.75) (rem' 1)
        , descendants
            [ code
                [ backgroundColor (rgb 240 240 240)
                , color (hex textColor)
                , fontSize (em 0.85)
                , padding2 (em 0.125) (em 0.5)
                , borderRadius (rem' 0.125)
                ]
            ]
        ]
    , (.)
        "Meta"
        [ padding2 (rem' 0.75) (rem' 1)
        , borderTop3 (px 1) solid (rgb 222 222 222)
        , property "display" "flex"
        , property "justify-content" "space-between"
        ]
    , (.)
        "Package"
        [ color (hex stringColor)
        ]
    , (.)
        "Module"
        [ color (hex titleHighlightColor)
        ]
    , (.)
        "Debug"
        [ position absolute
        , property "z-index" "999"
        , marginTop (rem' -3)
        ]
    ]
