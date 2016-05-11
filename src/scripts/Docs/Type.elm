module Docs.Type exposing (..)

-- where

import Char
import Dict exposing (Dict)
import Html exposing (..)
import String
import Docs.Name as Name
import Utils.Code as Code exposing (arrow, colon, padded, space)


-- MODEL


type Type
    = Function (List Type) Type
    | Var String
    | Apply Name.Canonical (List Type)
    | Tuple (List Type)
    | Record (List ( String, Type )) (Maybe String)



-- TYPE TO FLAT HTML


type Context
    = Func
    | App
    | Other


toHtml : Name.Dictionary -> Context -> Type -> List (Html msg)
toHtml nameDict context tipe =
    let
        go ctx t =
            toHtml nameDict ctx t
    in
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
                        List.concatMap (\arg -> go Func arg ++ padded arrow) args
                in
                    maybeAddParens (argsHtml ++ go Func result)

            Var name ->
                [ text name ]

            Apply name [] ->
                [ Name.toLink nameDict name ]

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
                        List.concatMap (\arg -> space :: go App arg) args
                in
                    maybeAddParens (Name.toLink nameDict name :: argsHtml)

            Tuple args ->
                List.map (go Other) args
                    |> List.intersperse [ text ", " ]
                    |> List.concat
                    |> Code.addParens

            Record fields ext ->
                let
                    fieldsHtml =
                        List.map (fieldToHtml nameDict) fields
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



-- TODO: avoid the duplication which only exists because of the links with basePath


toHtmlWithBasePath : String -> Name.Dictionary -> Context -> Type -> List (Html msg)
toHtmlWithBasePath basePath nameDict context tipe =
    let
        go ctx t =
            toHtmlWithBasePath basePath nameDict ctx t
    in
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
                        List.concatMap (\arg -> go Func arg ++ padded arrow) args
                in
                    maybeAddParens (argsHtml ++ go Func result)

            Var name ->
                [ text name ]

            Apply name [] ->
                [ Name.toBaseLink basePath nameDict name ]

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
                        List.concatMap (\arg -> space :: go App arg) args
                in
                    maybeAddParens (Name.toBaseLink basePath nameDict name :: argsHtml)

            Tuple args ->
                List.map (go Other) args
                    |> List.intersperse [ text ", " ]
                    |> List.concat
                    |> Code.addParens

            Record fields ext ->
                let
                    fieldsHtml =
                        List.map (fieldToHtml nameDict) fields
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


fieldToHtml : Name.Dictionary -> ( String, Type ) -> List (Html msg)
fieldToHtml nameDict ( field, tipe ) =
    text field :: space :: colon :: space :: toHtml nameDict Other tipe



-- TYPE LENGTH


length : Context -> Type -> Int
length context tipe =
    case tipe of
        Function args result ->
            let
                parens =
                    case context of
                        Func ->
                            2

                        App ->
                            2

                        Other ->
                            0

                argLengths =
                    List.map (\t -> 4 + length Func t) args
            in
                parens + List.sum argLengths + length Func result

        Var name ->
            String.length name

        Apply { name } [] ->
            String.length name

        Apply { name } args ->
            let
                parens =
                    case context of
                        Func ->
                            0

                        App ->
                            2

                        Other ->
                            0

                argsLength =
                    List.sum (List.map (\t -> 1 + length App t) args)
            in
                parens + String.length name + argsLength

        Tuple args ->
            List.sum (List.map (\t -> 2 + length Other t) args)

        Record fields ext ->
            let
                fieldLength ( field, tipe ) =
                    String.length field + 3 + length Other tipe

                recordLength =
                    2 + List.sum (List.map (\ft -> 2 + fieldLength ft) fields)

                extLength =
                    case ext of
                        Nothing ->
                            0

                        Just extName ->
                            2 + String.length extName
            in
                recordLength + extLength



-- SEARCH


noPenalty : Float
noPenalty =
    0


lowPenalty : Float
lowPenalty =
    0.25


mediumPenalty : Float
mediumPenalty =
    0.5


highPenalty : Float
highPenalty =
    0.75


maxPenalty : Float
maxPenalty =
    1


distance : Type -> Type -> Float
distance needle hay =
    case ( needle, hay ) of
        {- Compare two functions `Function (List Type) Type`
           Functions get parsed like `a -> b` ~> `Function ([Var "a"]) (Var "b")`
           TODO: support three different comparisons
             - strict: length of arguments have to match
             - from beginning: concat args and result and compare the list
             - from end: concat args and result and compare the reversed list
           TODO: add some kind of mapping for vars in fuzzy calculations
        -}
        ( Function argsN resultN, Function argsH resultH ) ->
            let
                -- Handle special cases with singleton `Var` Type args
                argsDistance =
                    case ( argsN, argsH ) of
                        -- Compare `a -> r` and `b -> s`
                        ( [ Var n ], [ Var h ] ) ->
                            distanceName n h

                        -- Compare `a -> r` and `b -> c -> s`
                        -- This is the important special case.
                        ( [ Var n ], _ ) ->
                            mediumPenalty

                        -- The default case
                        _ ->
                            distanceList argsN argsH

                resultDistance =
                    distance resultN resultH
            in
                (argsDistance + resultDistance) / 2

        -- `Var String`
        -- `a` ~> `Var "a"`
        ( Var nameN, Var nameH ) ->
            distanceName nameN nameH

        -- Special cases for comparisons like `number` - `Float`
        ( Var nameN, Apply canonicalH _ ) ->
            distanceVarApply nameN canonicalH

        ( Apply canonicalN _, Var nameH ) ->
            distanceVarApply nameH canonicalN

        -- `Apply Name.Canonical (List Type)`
        -- `Foo.Bar a b` ~> `Apply { home = "Foo", name = "Bar" } ([Var "a", Var "b"])`
        ( Apply canonicalN argsN, Apply canonicalH argsH ) ->
            distanceApply ( canonicalN, argsN ) ( canonicalH, argsH )

        -- Tuple (List Type)
        -- `(a,b)` ~> `Tuple ([Var "a",Var "b"])`
        ( Tuple argsN, Tuple argsH ) ->
            distanceList argsN argsH

        -- TODO: Record (List ( String, Type )) (Maybe String)
        {- The incomparable case
           TODO: Find and add special cases
        -}
        _ ->
            maxPenalty


distanceList : List Type -> List Type -> Float
distanceList needle hay =
    let
        needelLength =
            List.length needle

        hayLength =
            List.length hay

        sharedLength =
            min needelLength hayLength

        maxLength =
            max needelLength hayLength

        diffLength =
            maxLength - sharedLength
    in
        List.map2 distance needle hay
            |> List.sum
            |> (flip (+)) (toFloat diffLength * maxPenalty)
            |> (flip (/)) (toFloat maxLength)


distanceName : String -> String -> Float
distanceName needle hay =
    if needle == hay then
        noPenalty
    else
        maxPenalty


distanceCanonical : Name.Canonical -> Name.Canonical -> Float
distanceCanonical needle hay =
    -- TODO: Also take `.home` into account.
    --distanceName needle.name hay.name
    if needle.name == hay.name then
        noPenalty
    else if String.contains needle.name hay.name then
        mediumPenalty
    else
        maxPenalty


distanceVarApply : String -> Name.Canonical -> Float
distanceVarApply varName applyName =
    let
        maybeReservedVarTypeList =
            Dict.get varName reserverdVars
    in
        case maybeReservedVarTypeList of
            Just typeList ->
                if List.any ((==) applyName.name) typeList then
                    lowPenalty
                else
                    maxPenalty

            Nothing ->
                mediumPenalty


distanceApply : ( Name.Canonical, List Type ) -> ( Name.Canonical, List Type ) -> Float
distanceApply ( canonicalN, argsN ) ( canonicalH, argsH ) =
    case ( argsN, argsH ) of
        ( [], [] ) ->
            distanceCanonical canonicalN canonicalH

        ( [], hd :: tl ) ->
            --distanceCanonical canonicalN canonicalH
            -- TODO: should we do this only for some specific types like `Maybe` and `Result`?
            -- TODO: check if this is a nice implementation (with regard to `min` and `+ lowPenalty`)
            min maxPenalty
                <| distance (Apply canonicalN argsN)
                    (Maybe.withDefault hd (List.head (List.reverse tl)))
                + lowPenalty

        _ ->
            (distanceCanonical canonicalN canonicalH + distanceList argsN argsH) / 2


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
