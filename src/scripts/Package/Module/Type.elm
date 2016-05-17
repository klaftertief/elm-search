module Package.Module.Type exposing (..)

-- where

import Char
import Dict exposing (Dict)
import Html exposing (..)
import Json.Decode as Decode exposing (Decoder, (:=))
import String
import Package.Module.Name as Name exposing (Name)
import Parse.Combinators exposing (..)
import Utils.Code as Code exposing (arrow, colon, padded, space)


type Type
    = Function (List Type) Type
    | Var String
    | Apply Name (List Type)
    | Tuple (List Type)
    | Record (List ( String, Type )) (Maybe String)


type Context
    = Func
    | App
    | Other


toHtml : Context -> Type -> List (Html msg)
toHtml context tipe =
        case tipe of
            Function args result ->
                let
                    maybeAddParens =
                        case context of
                            Func ->
                                Code.addParens

                            App ->
                                Code.addParens

                            Other ->
                                identity

                    argsHtml =
                        List.concatMap (\arg -> toHtml Func arg ++ padded arrow) args
                in
                    maybeAddParens (argsHtml ++ toHtml Func result)

            Var name ->
                [ text name ]

            Apply name [] ->
                [ text (Name.nameToString name) ]

            Apply name args ->
                let
                    maybeAddParens =
                        case context of
                            Func ->
                                identity

                            App ->
                                Code.addParens

                            Other ->
                                identity

                    argsHtml =
                        List.concatMap (\arg -> space :: toHtml App arg) args
                in
                    maybeAddParens (text (Name.nameToString name) :: argsHtml)

            Tuple args ->
                List.map (toHtml Other) args
                    |> List.intersperse [ text ", " ]
                    |> List.concat
                    |> Code.addParens

            Record fields ext ->
                let
                    fieldsHtml =
                        List.map fieldToHtml fields
                            |> List.intersperse [ text ", " ]
                            |> List.concat

                    recordInsides =
                        case ext of
                            Nothing ->
                                fieldsHtml

                            Just extName ->
                                text extName :: text " | " :: fieldsHtml
                in
                    text "{ " :: recordInsides ++ [ text " }" ]

fieldToHtml : ( String, Type ) -> List (Html msg)
fieldToHtml ( field, tipe ) =
    text field :: space :: colon :: space :: toHtml Other tipe


decoder : Decoder Type
decoder =
    Decode.customDecoder Decode.string parse



-- NORMALIZE


reserverdVars : Dict String (List String)
reserverdVars =
    Dict.empty
        |> Dict.insert "number" [ "Float", "Int" ]
        |> Dict.insert "comparable" [ "Float", "Int", "Char", "String" ]
        |> Dict.insert "appendable" [ "String", "List" ]


type alias Mapping =
    Dict String String


defaultMapping : Mapping
defaultMapping =
    Dict.keys reserverdVars
        |> List.map (\v -> ( v, v ))
        |> Dict.fromList


nextMappingValue : Mapping -> String
nextMappingValue mapping =
    let
        base =
            (Dict.size mapping) - (Dict.size defaultMapping)

        code =
            (base % 26) + (Char.toCode 'a')

        string =
            String.fromChar (Char.fromCode code)

        times =
            (base // 26) + 1
    in
        String.repeat times string


updateMapping : Type -> Mapping -> Mapping
updateMapping tipe mapping =
    let
        updateMappingFor name =
            if Dict.member name mapping then
                mapping
            else
                Dict.insert name
                    (nextMappingValue mapping)
                    mapping
    in
        case tipe of
            Function args result ->
                List.foldl updateMapping mapping (List.append args [ result ])

            Var name ->
                updateMappingFor name

            Apply name args ->
                List.foldl updateMapping mapping args

            Tuple args ->
                List.foldl updateMapping mapping args

            Record fields ext ->
                List.foldl updateMapping mapping (List.map (\( _, t ) -> t) fields)


normalize : Type -> Type
normalize tipe =
    normalizeWithMapping (updateMapping tipe defaultMapping) tipe


normalizeWithMapping : Mapping -> Type -> Type
normalizeWithMapping mapping tipe =
    let
        normalize' =
            normalizeWithMapping mapping
    in
        case tipe of
            Function args result ->
                Function (List.map normalize' args)
                    (normalize' result)

            Var name ->
                let
                    name' =
                        case Dict.get name mapping of
                            Just n ->
                                n

                            Nothing ->
                                name
                in
                    Var name'

            Apply name args ->
                Apply name (List.map normalize' args)

            Tuple args ->
                Tuple (List.map normalize' args)

            Record fields ext ->
                Record (List.map (\( k, v ) -> ( k, normalize' v )) fields) ext



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
            middle (ignore1 (char '{') spaces)
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
                    [ map2 (\t rest -> Record (( lowerName, t ) :: rest) Nothing)
                        (ignore2 (char ':') spaces tipe)
                        (commasLeading field)
                    , map (\fields -> Record fields (Just lowerName))
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
                <| middle (ignore1 (char '(') spaces)
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
