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
             , ( "-> T -> T"
               , Partial.LambdaTo
                    (Partial.Lambda
                        (Partial.Type "T" [])
                        (Partial.Type "T" [])
                    )
               )
             , ( "T -> T"
               , Partial.Lambda
                    (Partial.Type "T" [])
                    (Partial.Type "T" [])
               )
             , ( "a -> a"
               , Partial.Lambda
                    (Partial.Var "a")
                    (Partial.Var "a")
               )
             , ( "(a -> a)"
               , Partial.Lambda
                    (Partial.Var "a")
                    (Partial.Var "a")
               )
             , ( "(a -> a) -> a"
               , Partial.Lambda
                    (Partial.Lambda
                        (Partial.Var "a")
                        (Partial.Var "a")
                    )
                    (Partial.Var "a")
               )
             , ( "a -> (a -> a)"
               , Partial.Lambda
                    (Partial.Var "a")
                    (Partial.Lambda
                        (Partial.Var "a")
                        (Partial.Var "a")
                    )
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
