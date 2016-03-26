module Tests (..) where

import ElmTest exposing (..)
import Docs.Type as Type
import Component.PackageDocs as Docs


_ =
  Debug.log "fTest" (Type.distanceF (Docs.stringToType "String") (Docs.stringToType "Int"))


flipQuery =
  "(x -> y -> z) -> y -> x -> z"


flipQueryAlt =
  "(m -> n -> o) -> n -> m -> o"


flipType =
  Docs.stringToType flipQuery


flipTypeAlt =
  Docs.stringToType flipQuery


listFoldlQuery =
  "(a -> b -> b) -> b -> List a -> b"


listFoldlType =
  Docs.stringToType listFoldlQuery


resultAndThenQuery =
  "Result x a -> (a -> Result x b) -> Result x b"


resultAndThenType =
  Docs.stringToType resultAndThenQuery


all : Test
all =
  suite
    "All"
    [ identicals, fuzzyPositives ]


identicals : Test
identicals =
  suite
    "Identical types"
    [ test "flip" (assertEqual (Type.distance flipType flipType) 0)
    , test "flip - normalized" (assertEqual (Type.distance (Type.normalize flipType) (Type.normalize flipTypeAlt)) 0)
    , test "List.foldl" (assertEqual (Type.distance listFoldlType listFoldlType) 0)
    , test "Result.andThen" (assertEqual (Type.distance resultAndThenType resultAndThenType) 0)
    ]


fuzzyPositives : Test
fuzzyPositives =
  suite
    "Fuzzy positives"
    [ test "Result - only Ok"
        <| ElmTest.assert
        <| Type.distance
            (Docs.stringToType "String -> Int")
            (Docs.stringToType "String -> Result Error Int")
        < 3
    , test "Mix type variable - a <-> S"
        <| ElmTest.assert
        <| Type.distance
            (Docs.stringToType "String -> a")
            (Docs.stringToType "String -> String")
        < 2
    , test "Mix type variable - S <-> a"
        <| ElmTest.assert
        <| Type.distance
            (Docs.stringToType "String -> String")
            (Docs.stringToType "String -> a")
        < 2
    ]
