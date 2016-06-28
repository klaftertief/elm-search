module Parse.Type exposing (parse, decoder)

import Char
import String
import Json.Decode as Decode exposing (Decoder)
import Combine exposing (..)
import Combine.Char exposing (..)
import Docs.Name as Name exposing (Name)
import Docs.Type exposing (Type(..))


decoder : Decoder Type
decoder =
    Decode.customDecoder Decode.string parse


parse : String -> Result.Result String Type
parse tipeString =
    Combine.parse tipe tipeString
        |> fst
        |> Result.formatError (String.join ", ")



-- TYPE VARIABLES


var : Parser Type
var =
    map Var (elmVarWith lower)



-- TYPE APPLICATIONS


name : Parser Name
name =
    nameHelp []


nameHelp : List String -> Parser Name
nameHelp seen =
    elmVarWith upper
        `andThen` \str ->
                    choice
                        [ ignore1 (char '.') (nameHelp (str :: seen))
                        , succeed (Name (String.join "." (List.reverse seen)) str)
                        ]


apply : Parser Type
apply =
    rec
        <| \_ ->
            map2 Apply name (many (ignore1 spaces applyTerm))


applyTerm : Parser Type
applyTerm =
    rec
        <| \_ ->
            choice [ var, map (\n -> Apply n []) name, record, parenTipe ]



-- RECORDS


record : Parser Type
record =
    rec
        <| \_ ->
            middle (ignore1 (char '{') spaces)
                (choice
                    [ elmVarWith lower `andThen` recordHelp
                    , succeed (Record [] Nothing)
                    ]
                )
                (ignore1 spaces (char '}'))


recordHelp : String -> Parser Type
recordHelp lowerName =
    rec
        <| \_ ->
            ignore1 spaces
                <| choice
                    [ map2 (\t rest -> Record (( lowerName, t ) :: rest) Nothing)
                        (ignore2 (char ':') spaces tipe)
                        (commasLeading field)
                    , map (\fields -> Record fields (Just lowerName))
                        (ignore2 (char '|') spaces (map2 (::) field (commasLeading field)))
                    ]


field : Parser ( String, Type )
field =
    rec
        <| \_ ->
            map2 (,) (elmVarWith lower) (ignore3 spaces (char ':') spaces tipe)



-- FUNCTIONS


tipe : Parser Type
tipe =
    rec
        <| \_ ->
            map2 (buildFunction []) tipeTerm arrowTerms


buildFunction : List Type -> Type -> List Type -> Type
buildFunction args currentType remainingTypes =
    case remainingTypes of
        [] ->
            if List.isEmpty args then
                currentType
            else
                Function (List.reverse args) currentType

        t :: ts ->
            buildFunction (currentType :: args) t ts


arrowTerms : Parser (List Type)
arrowTerms =
    rec
        <| \_ ->
            many (ignore3 spaces (string "->") spaces tipeTerm)


tipeTerm : Parser Type
tipeTerm =
    rec
        <| \_ ->
            choice [ var, apply, record, parenTipe ]


parenTipe : Parser Type
parenTipe =
    rec
        <| \_ ->
            map tuplize
                <| middle (ignore1 (char '(') spaces)
                    (choice
                        [ map2 (::) tipe (commasLeading tipe)
                        , succeed []
                        ]
                    )
                    (ignore1 spaces (char ')'))


tuplize : List Type -> Type
tuplize args =
    case args of
        [ t ] ->
            t

        _ ->
            Tuple args



-- HELPERS


elmVarWith : Parser Char -> Parser String
elmVarWith starter =
    map2 (::) starter (many varChar)
        |> map String.fromList


varChar : Parser Char
varChar =
    satisfy (\c -> Char.isLower c || Char.isUpper c || c == '_' || c == '\'' || Char.isDigit c)


spaces : Parser ()
spaces =
    map (always ()) (many space)


commasLeading : Parser a -> Parser (List a)
commasLeading parser =
    many (ignore3 spaces (char ',') spaces parser)



-- UTILS


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 func parserA parserB =
    parserA
        `andThen` \a ->
                    parserB
                        `andThen` \b ->
                                    succeed (func a b)


map3 : (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
map3 func parserA parserB parserC =
    parserA
        `andThen` \a ->
                    parserB
                        `andThen` \b ->
                                    parserC
                                        `andThen` \c ->
                                                    succeed (func a b c)


map4 : (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
map4 func parserA parserB parserC parserD =
    parserA
        `andThen` \a ->
                    parserB
                        `andThen` \b ->
                                    parserC
                                        `andThen` \c ->
                                                    parserD
                                                        `andThen` \d ->
                                                                    succeed (func a b c d)


ignore1 : Parser x -> Parser a -> Parser a
ignore1 x parser =
    map2 (\_ a -> a) x parser


ignore2 : Parser x -> Parser y -> Parser a -> Parser a
ignore2 x y parser =
    map3 (\_ _ a -> a) x y parser


ignore3 : Parser x -> Parser y -> Parser z -> Parser a -> Parser a
ignore3 x y z parser =
    map4 (\_ _ _ a -> a) x y z parser


middle : Parser x -> Parser a -> Parser y -> Parser a
middle x parser y =
    map3 (\_ a _ -> a) x parser y
