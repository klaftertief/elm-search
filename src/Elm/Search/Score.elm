module Elm.Search.Score exposing
    ( Score
    , add
    , byComment
    , byName
    , toFloat
    )


type Score
    = Distance Float
    | Possible
    | Impossible


add : Score -> Score -> Score
add s1 s2 =
    case ( s1, s2 ) of
        ( Distance d1, Distance d2 ) ->
            Distance (d1 + d2)

        ( Possible, Distance _ ) ->
            s2

        ( Distance _, Possible ) ->
            s1

        ( Possible, Possible ) ->
            Possible

        ( Impossible, _ ) ->
            Impossible

        ( _, Impossible ) ->
            Impossible


toFloat : Score -> Float
toFloat score =
    case score of
        Distance distance ->
            distance

        Possible ->
            1 / 0

        Impossible ->
            1 / 0


byName : String -> { a | name : String } -> Score
byName query { name } =
    if query == name then
        Distance 0

    else if String.toLower query == String.toLower name then
        Distance 3

    else if String.contains (String.toLower query) (String.toLower name) then
        Distance 7

    else
        Impossible


byComment : String -> { a | comment : String } -> Score
byComment query { comment } =
    if String.length query < 4 then
        Possible

    else if query == comment then
        Distance 0

    else if String.toLower query == String.toLower comment then
        Distance 1

    else if String.contains (String.toLower query) (String.toLower comment) then
        Distance 3

    else
        Impossible
