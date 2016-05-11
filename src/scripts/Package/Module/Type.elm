module Package.Module.Type exposing (..)

-- where

import Char
import Json.Decode as Decode exposing (Decoder, (:=))
import String
import Package.Module.Name as Name exposing (Name)
import Parse.Combinators exposing (..)


type Type
  = Function (List Type) Type
  | Var String
  | Apply Name (List Type)
  | Tuple (List Type)
  | Record (List ( String, Type )) (Maybe String)


decoder : Decoder Type
decoder =
  Decode.customDecoder Decode.string parse



-- PARSE


parse : String -> Result String Type
parse tipeString =
  run tipe tipeString



-- HELPERS


elmVarWith : Parser Char -> Parser String
elmVarWith starter =
  map2 (::) starter (zeroOrMore varChar)
    |> map String.fromList


varChar : Parser Char
varChar =
  satisfy (\c -> Char.isLower c || Char.isUpper c || c == '_' || c == '\'' || Char.isDigit c)


spaces : Parser ()
spaces =
  map (always ()) (zeroOrMore (char ' '))


commasLeading : Parser a -> Parser (List a)
commasLeading parser =
  zeroOrMore (ignore3 spaces (char ',') spaces parser)



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
                oneOf
                  [ ignore1 (char '.') (nameHelp (str :: seen))
                  , succeed (Name (String.join "." (List.reverse seen)) str)
                  ]


apply : Parser Type
apply =
  lazy
    <| \_ ->
        map2 Apply name (zeroOrMore (ignore1 spaces applyTerm))


applyTerm : Parser Type
applyTerm =
  lazy
    <| \_ ->
        oneOf [ var, map (\n -> Apply n []) name, record, parenTipe ]



-- RECORDS


record : Parser Type
record =
  lazy
    <| \_ ->
        middle
          (ignore1 (char '{') spaces)
          (oneOf
            [ elmVarWith lower `andThen` recordHelp
            , succeed (Record [] Nothing)
            ]
          )
          (ignore1 spaces (char '}'))


recordHelp : String -> Parser Type
recordHelp lowerName =
  lazy
    <| \_ ->
        ignore1 spaces
          <| oneOf
              [ map2
                  (\t rest -> Record (( lowerName, t ) :: rest) Nothing)
                  (ignore2 (char ':') spaces tipe)
                  (commasLeading field)
              , map
                  (\fields -> Record fields (Just lowerName))
                  (ignore2 (char '|') spaces (map2 (::) field (commasLeading field)))
              ]


field : Parser ( String, Type )
field =
  lazy
    <| \_ ->
        map2 (,) (elmVarWith lower) (ignore3 spaces (char ':') spaces tipe)



-- FUNCTIONS


tipe : Parser Type
tipe =
  lazy
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
  lazy
    <| \_ ->
        zeroOrMore (ignore3 spaces (string "->") spaces tipeTerm)


tipeTerm : Parser Type
tipeTerm =
  lazy
    <| \_ ->
        oneOf [ var, apply, record, parenTipe ]


parenTipe : Parser Type
parenTipe =
  lazy
    <| \_ ->
        map tuplize
          <| middle
              (ignore1 (char '(') spaces)
              (oneOf
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
