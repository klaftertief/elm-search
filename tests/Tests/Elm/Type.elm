module Tests.Elm.Type exposing (suite)

import Elm.Type as Type exposing (Type)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Types"
        [ describe "parse"
            ([ ( "a"
               , Type.Var "a"
               )
             , ( "T"
               , Type.Type "T" []
               )
             , ( "a -> b"
               , Type.Lambda (Type.Var "a") (Type.Var "b")
               )
             , ( "a -> b -> c"
               , Type.Lambda (Type.Var "a")
                    (Type.Lambda (Type.Var "b") (Type.Var "c"))
               )
             , ( "(a -> b) -> c"
               , Type.Lambda (Type.Lambda (Type.Var "a") (Type.Var "b")) (Type.Var "c")
               )
             ]
                |> List.map testParse
            )
        ]


testParse : ( String, Type ) -> Test
testParse ( string, exp ) =
    test string <|
        \_ ->
            case Type.parse string of
                Ok v ->
                    v |> Expect.equal exp

                Err err ->
                    Expect.fail (Debug.toString err)
