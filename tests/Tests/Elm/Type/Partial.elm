module Tests.Elm.Type.Partial exposing (suite)

import Elm.Type.Partial as Partial exposing (Type)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Partial Types"
        [ describe "parse"
            ([ ( "T -> "
               , Partial.LambdaFrom (Partial.Type "T" [])
               )
             , ( "T -> T ->"
               , Partial.Lambda (Partial.Type "T" [])
                    (Partial.LambdaFrom (Partial.Type "T" []))
               )
             , ( "-> T"
               , Partial.LambdaTo (Partial.Type "T" [])
               )
             ]
                |> List.map testParse
            )
        ]


testParse : ( String, Type ) -> Test
testParse ( string, exp ) =
    test string <|
        \_ ->
            case Partial.parse string of
                Ok v ->
                    v |> Expect.equal exp

                Err err ->
                    Expect.fail (Debug.toString err)
