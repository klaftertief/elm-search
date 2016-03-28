module TestsF (..) where

import ElmTest exposing (..)
import Docs.Type as Type
import Component.PackageDocs as Docs


varSuite : Test
varSuite =
  suite
    "Single `Var` comparisons"
    <| List.map
        descriptionToTest
        [ Description
            "Equal Var should not have a penalty"
            "a"
            "a"
            Type.noPenalty
        , Description
            "Not equal Var should have max penalty"
            "a"
            "b"
            Type.maxPenalty
        ]


applySuite : Test
applySuite =
  suite
    "Single `Apply` comparisons"
    <| List.map
        descriptionToTest
        [ Description
            "Equal simple Apply should not have a penalty"
            "String"
            "String"
            Type.noPenalty
        , Description
            "Not equal simple Apply should have max penalty"
            "Int"
            "String"
            Type.maxPenalty
        , Description
            "Simple Apply as last arg in wrapped Apply should have low penalty"
            "Int"
            "Maybe Int"
            Type.lowPenalty
        , Description
            "Simple Apply as last arg in wrapped Apply should have low penalty"
            "Int"
            "Result String Int"
            Type.lowPenalty
        , Description
            "Simple Apply without match in wrapped Apply should have maximum penalty"
            "String"
            "Maybe Int"
            Type.maxPenalty
        ]


varApplySuite : Test
varApplySuite =
  suite
    "Single `Var` - `Apply` comparisons"
    <| List.map
        descriptionToTest
        [ Description
            "Var - Apply comparison should have medium penalty"
            "a"
            "Int"
            Type.mediumPenalty
        , Description
            "Apply - Var comparison should have medium penalty"
            "Int"
            "a"
            Type.mediumPenalty
        , Description
            "reserved Var - Apply comparison with match should have low penalty"
            "number"
            "Int"
            Type.lowPenalty
        , Description
            "reserved Var - Apply comparison with match should have low penalty"
            "number"
            "Float"
            Type.lowPenalty
        , Description
            "reserved Var - Apply comparison without match should have low penalty"
            "number"
            "String"
            Type.maxPenalty
        ]


functionSuite : Test
functionSuite =
  suite
    "Basic `Function` comparisons"
    <| List.map
        descriptionToTest
        [ Description
            "Equal Var Functions should have no penalty"
            "a -> b"
            "a -> b"
            Type.noPenalty
        , Description
            "Equal Apply Functions should have no penalty"
            "String -> Int"
            "String -> Int"
            Type.noPenalty
        , Description
            "fuzzy Maybe"
            "String -> Int"
            "String -> Maybe Int"
            -- 0.125
            ((Type.noPenalty + Type.lowPenalty) / 2)
        , Description
            "fuzzy Result"
            "String -> Int"
            "String -> Result Error Int"
            -- 0.125
            ((Type.noPenalty + Type.lowPenalty) / 2)
        , Description
            "fuzzy var Result"
            "String -> Int"
            "String -> Result x Int"
            -- 0.125
            ((Type.noPenalty + Type.lowPenalty) / 2)
        , Description
            "fuzzy Function args"
            "a -> Int"
            "String -> Int"
            0.25
        , Description
            "fuzzy Function args"
            "String -> Int"
            "a -> Int"
            0.25
        , Description
            "fuzzy Function result"
            "String -> a"
            "String -> Int"
            0.25
        , Description
            "fuzzy Function result"
            "String -> Int"
            "String -> a"
            0.25
        , Description
            "fuzzy Function args"
            "a -> Int"
            "Int -> Int -> Int"
            0.25
        , Description
            "TODO: fuzzy Function args"
            "a -> Int"
            "a -> b -> Int"
            666
        , Description
            "TODO: fuzzy Function args"
            "a -> Int"
            "b -> c -> Int"
            666
        , Description
            "TODO: ??? fuzzy Function args Apply result"
            "a -> b -> Int"
            "a -> b"
            666
        , Description
            "TODO: ??? fuzzy Function args Var result"
            "a -> b -> c"
            "a -> b"
            666
        ]


listSuite : Test
listSuite =
  suite
    "(List Type) comparisons (hijacking Tuple)"
    <| List.map
        descriptionToTest
        [ Description
            "Equal Var 1-Tuple should not have a penalty"
            "(a)"
            "(a)"
            Type.noPenalty
        , Description
            "Equal Var 2-Tuple should not have a penalty"
            "(a, b)"
            "(a, b)"
            Type.noPenalty
        , Description
            "Equal Var 3-Tuple should not have a penalty"
            "(a, b, c)"
            "(a, b, c)"
            Type.noPenalty
        , Description
            "Not equal Var 2-Tuple should have max penalty"
            "(a)"
            "(b)"
            Type.maxPenalty
        , Description
            "1 max and 1 no should have medium penalty"
            "(a, b)"
            "(a, c)"
            Type.mediumPenalty
        , Description
            "2 max and 1 no should have 1/3 penalty"
            "(a, b, c)"
            "(a, b, d)"
            (Type.maxPenalty / 3)
        , Description
            "1-tuple with no match no should have max penalty"
            "(d)"
            "(a, b, c)"
            Type.maxPenalty
        , Description
            "2-tuple with 1 match in 4 should have high penalty"
            "(a, r)"
            "(a, b, c, d)"
            Type.highPenalty
        , Description
            "2-tuple with 2 matches in 4 should have medium penalty"
            "(a, b)"
            "(a, b, c, d)"
            Type.mediumPenalty
        , Description
            "2-tuple with 2 matches in 3 should have 1/3 penalty"
            "(a, b)"
            "(a, b, c)"
            (Type.maxPenalty / 3)
        , Description
            "3-tuple with 3 matches in 4 should have low penalty"
            "(a, b, c)"
            "(a, b, c, d)"
            Type.lowPenalty
        , Description
            "3-tuple with 1 matche in 4 should have high penalty"
            "(a, x, y)"
            "(a, b, c, d)"
            Type.highPenalty
        ]


all : Test
all =
  suite
    "All distances"
    [ varSuite
    , applySuite
    , varApplySuite
    , functionSuite
    , listSuite
    ]


type alias Description =
  { title : String
  , needle : String
  , hay : String
  , expected : Float
  }


descriptionToTest : Description -> Test
descriptionToTest { title, needle, hay, expected } =
  test (title ++ "; needle: " ++ needle ++ "; hay: " ++ hay)
    <| assertEqual
        expected
        (Type.distance
          (Docs.stringToType needle)
          (Docs.stringToType hay)
        )
