module Elm.Type.Partial exposing
    ( Type(..)
    , parse
    )

import Char
import Parser exposing (..)
import Set


type Type
    = Lambda Type Type
    | LambdaFrom Type
    | LambdaTo Type
    | Var String
    | Type String (List Type)
    | TypeQual String String (List Type)
    | Record (Maybe String) (List ( String, Type ))
    | Unit
    | Pair Type Type
    | Triple Type Type Type
    | VariableType String (List Type)
    | Unknown String
    | PartialRecord (Maybe String) (List ( String, Type )) String


parse : String -> Result (List DeadEnd) Type
parse source =
    Parser.run term source


term : Parser Type
term =
    lazy
        (\() ->
            oneOf
                [ backtrackable typeclassVar
                , term1
                ]
                |> andThen
                    (\t ->
                        oneOf
                            [ succeed (Lambda t)
                                |. symbol "->"
                                |. spaces
                                |= orPartial term
                            , succeed (Lambda t)
                                |. symbol "->"
                                |. spaces
                                |= orPartial term
                            , succeed (Lambda t)
                                |. symbol "-"
                                |. spaces
                                |= orPartial (problem "only want partial here")
                            , succeed t
                            ]
                    )
        )


orPartial p =
    oneOf
        [ backtrackable p
        , succeed Unknown |= getChompedString (chompWhile (\c -> c /= ')') |. end)
        , succeed Unknown |= getChompedString (chompWhile (\_ -> True) |. end)
        ]


term1 =
    lazy
        (\() ->
            skip spaces <|
                (oneOf
                    [ record
                    , typeWithOptArgs
                    , tvar
                    , backtrackable <| expr
                    , paren term
                    ]
                    |. spaces
                )
        )


tvar =
    oneOf
        [ succeed Var |= lower
        , typeclassVar
        ]


expr =
    oneOf
        [ backtrackable <| unit
        , backtrackable <| pair
        , triple
        ]


paren p =
    succeed identity
        |. symbol "("
        |. spaces
        |= p
        |. spaces
        |. oneOf [ symbol ")", end ]


typeWithOptArgs =
    (succeed identity
        |. spaces
        |= qname
        |. spaces
        |> andThen
            (\v ->
                case v |> String.split "." |> List.reverse of
                    [] ->
                        problem "app with no args"

                    [ a ] ->
                        succeed (Type a)

                    last :: rest ->
                        succeed (TypeQual (String.join "." (List.reverse rest)) last)
            )
    )
        |. spaces
        |= eatArgs
        |. spaces


typeclassVar =
    succeed VariableType
        |. spaces
        |= lower
        |. spaces
        |= (eatArgs
                |> andThen
                    (\args ->
                        case args of
                            [] ->
                                problem "need at least one arg"

                            _ ->
                                succeed args
                    )
           )
        |. spaces


qname =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '.'
        , reserved = Set.empty
        }


eatArgs : Parser (List Type)
eatArgs =
    eatArgsHelp []


eatArgsHelp args =
    oneOf
        [ succeed identity
            |. spaces
            |= term1
            |. spaces
            |> andThen (\v -> eatArgsHelp (args ++ [ v ]))
        , succeed args
        ]


upper =
    variable
        { start = Char.isUpper
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


lower =
    variable
        { start = \c -> Char.isLower c || c == '_'
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


unit =
    succeed Unit
        |. spaces
        |. symbol "()"
        |. spaces


pair =
    succeed Pair
        |. symbol "("
        |. spaces
        |= term
        |. symbol ","
        |. spaces
        |= term
        |. symbol ")"
        |. spaces


triple =
    succeed Triple
        |. symbol "("
        |. spaces
        |= term
        |. symbol ","
        |. spaces
        |= term
        |. symbol ","
        |. spaces
        |= term
        |. symbol ")"
        |. spaces


record =
    oneOf
        [ backtrackable fullRecord
        , unfinishedRecord
        ]


{-| if the last `}` is missing, the user might want to write more fields, so treat this as an "at least these fields" match
-}
unfinishedRecord =
    oneOf
        [ backtrackable <|
            succeed (\r -> PartialRecord (Just r))
                |. symbol "{"
                |. spaces
                |= oneOf [ lower, succeed "" ]
                |. spaces
                |. symbol "|"
        , succeed (PartialRecord Nothing)
            |. symbol "{"
            |. spaces
        ]
        |= sequence
            { start = ""
            , separator = ","
            , end = ""
            , spaces = spaces
            , item = recordField
            , trailing = Optional -- not correct elm syntax, but we know what you meant
            }
        |= oneOf
            [ backtrackable <|
                succeed identity
                    |. spaces
                    |= getChompedString (chompWhile (\c -> c /= '}'))
                    |. spaces
                    |. oneOf [ symbol "}", succeed () ]
                    |. spaces
                    |. end
            , succeed identity
                |. spaces
                |= getChompedString (chompWhile (\_ -> True) |. end)
            ]


fullRecord =
    oneOf
        [ backtrackable <|
            succeed (\r -> Record (Just r))
                |. symbol "{"
                |. spaces
                |= lower
                |. spaces
                |. symbol "|"
        , succeed (Record Nothing)
            |. symbol "{"
            |. spaces
        ]
        |= sequence
            { start = ""
            , separator = ","
            , end = "}"
            , spaces = spaces
            , item = recordField
            , trailing = Optional -- not correct elm syntax, but we know what you meant
            }


recordField =
    oneOf
        [ backtrackable <|
            succeed Tuple.pair
                |. spaces
                |= lower
                |. spaces
                |. oneOf [ symbol ":", symbol "=" ]
                |. spaces
                |= orPartial term
        , succeed Tuple.pair
            |. spaces
            |= lower
            |. spaces
            |= succeed (Unknown "")
        ]



-- non-exposed useful functions from elm/parser


skip iParser kParser =
    map2 revAlways iParser kParser


map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 fn a b =
    succeed fn |= a |= b


revAlways : a -> b -> b
revAlways _ b =
    b
