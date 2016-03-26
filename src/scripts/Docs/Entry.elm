module Docs.Entry (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
import Regex
import String
import Docs.Name as Name
import Docs.Type as Type exposing (Type)
import Utils.Code exposing (arrow, colon, equals, keyword, padded, space)
import Utils.Markdown as Markdown


-- MODEL


type alias Model tipe =
  { name : String
  , info : Info tipe
  , docs : String
  }


type Info tipe
  = Value tipe (Maybe Fixity)
  | Union
      { vars : List String
      , tags : List (Tag tipe)
      }
  | Alias
      { vars : List String
      , tipe : tipe
      }


type alias Tag tipe =
  { tag : String
  , args : List tipe
  }


type alias Fixity =
  { precedence : Int
  , associativity : String
  }


encode : Model String -> Encode.Value
encode model =
  Encode.object
    [ ( "name", Encode.string model.name )
    , ( "comment", Encode.string model.docs )
    , ( "type", encodeInfo model.info )
    ]


encodeInfo : Info String -> Encode.Value
encodeInfo info =
  case info of
    Value tipe fixity ->
      Encode.string tipe

    _ ->
      Encode.string "TODO"



-- MAP


map : (a -> b) -> Model a -> Model b
map func model =
  let
    newInfo =
      case model.info of
        Value tipe fixity ->
          Value (func tipe) fixity

        Union { vars, tags } ->
          Union { vars = vars, tags = List.map (tagMap func) tags }

        Alias { vars, tipe } ->
          Alias { vars = vars, tipe = func tipe }
  in
    { model | info = newInfo }


tagMap : (a -> b) -> Tag a -> Tag b
tagMap func tag =
  { tag | args = List.map func tag.args }



-- FILTER


nameDistance : String -> Model Type -> Float
nameDistance query model =
  case model.info of
    Value tipe _ ->
      if query == model.name then
        Type.noPenalty
      else if String.contains query model.name then
        Type.lowPenalty
      else
        Type.maxPenalty

    _ ->
      Type.maxPenalty



-- Only find `Value` entries, no `Union` or `Alias`


typeDistance : Type -> Model Type -> Float
typeDistance queryType model =
  case model.info of
    Value tipe _ ->
      Type.distance queryType tipe

    _ ->
      Type.maxPenalty



-- STRING VIEW


stringView : Model String -> Html
stringView model =
  let
    annotation =
      case model.info of
        Value tipe _ ->
          [ nameToLink model.name :: padded colon ++ [ text tipe ] ]

        Union _ ->
          [ [ text "Union annotation not supported" ] ]

        Alias _ ->
          [ [ text "Alias annotation not supported" ] ]
  in
    div
      [ class "docs-entry", id model.name ]
      [ annotationBlock annotation
      , div [ class "docs-comment" ] [ Markdown.block model.docs ]
      ]



-- TYPE VIEW


(=>) : a -> b -> ( a, b )
(=>) =
  (,)



-- TODO: DRY this up with the existing "normal" typeView, mainly with regard to support absolute links via the basePath od the Context.


typeViewSearch : String -> Name.Canonical -> Name.Dictionary -> Model Type -> Html
typeViewSearch basePath ({ home, name } as canonical) nameDict model =
  let
    path =
      "http://package.elm-lang.org/packages/" ++ basePath

    modulePath =
      path
        ++ "/"
        ++ String.map
            (\c ->
              if c == '.' then
                '-'
              else
                c
            )
            home

    annotation =
      case model.info of
        Value tipe _ ->
          valueAnnotationSearch path canonical nameDict model.name tipe

        Union _ ->
          [ [ text "Union annotation not supported" ] ]

        Alias _ ->
          [ [ text "Alias annotation not supported" ] ]

    description =
      Maybe.withDefault
        ""
        (List.head (String.split "\n\n" model.docs))
  in
    div
      [ class "searchResult" ]
      [ annotationBlock annotation
      , div [ class "searchDescription" ] [ Markdown.block description ]
      , div
          [ class "searchMeta" ]
          [ a
              [ href path, class "searchPackage" ]
              [ text basePath ]
          , a
              [ href modulePath, class "searchModule" ]
              [ text canonical.home ]
          ]
      ]


typeView : Name.Dictionary -> Model Type -> Html
typeView nameDict model =
  let
    annotation =
      case model.info of
        Value tipe _ ->
          valueAnnotation nameDict model.name tipe

        Union _ ->
          [ [ text "Union annotation not supported" ] ]

        Alias _ ->
          [ [ text "Alias annotation not supported" ] ]
  in
    div
      [ class "docs-entry", id model.name ]
      [ annotationBlock annotation
      , div [ class "docs-comment" ] [ Markdown.block model.docs ]
      ]


annotationBlock : List (List Html) -> Html
annotationBlock bits =
  div
    [ class "searchAnnotation" ]
    [ pre
        []
        [ code
            []
            (List.concat (List.intersperse [ text "\n" ] bits))
        ]
    ]


nameToLink : String -> Html
nameToLink name =
  let
    humanName =
      if Regex.contains operator name then
        "(" ++ name ++ ")"
      else
        name
  in
    a [ style [ "font-weight" => "bold" ], href ("#" ++ name) ] [ text humanName ]


operator : Regex.Regex
operator =
  Regex.regex "^[^a-zA-Z0-9]+$"



-- VALUE ANNOTATIONS


valueAnnotationSearch : String -> Name.Canonical -> Name.Dictionary -> String -> Type -> List (List Html)
valueAnnotationSearch basePath canonical nameDict name tipe =
  case tipe of
    Type.Function args result ->
      if String.length name + 3 + Type.length Type.Other tipe > 64 then
        [ Name.toBaseLink basePath nameDict canonical ] :: longFunctionAnnotationSearch basePath nameDict args result
      else
        [ (Name.toBaseLink basePath nameDict canonical) :: padded colon ++ Type.toHtmlWithBasePath basePath nameDict Type.Other tipe ]

    _ ->
      [ Name.toBaseLink basePath nameDict canonical :: padded colon ++ Type.toHtmlWithBasePath basePath nameDict Type.Other tipe ]


valueAnnotation : Name.Dictionary -> String -> Type -> List (List Html)
valueAnnotation nameDict name tipe =
  case tipe of
    Type.Function args result ->
      if String.length name + 3 + Type.length Type.Other tipe > 64 then
        [ nameToLink name ] :: longFunctionAnnotation nameDict args result
      else
        [ nameToLink name :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]

    _ ->
      [ nameToLink name :: padded colon ++ Type.toHtml nameDict Type.Other tipe ]


longFunctionAnnotation : Name.Dictionary -> List Type -> Type -> List (List Html)
longFunctionAnnotation nameDict args result =
  let
    tipeHtml =
      List.map (Type.toHtml nameDict Type.Func) (args ++ [ result ])

    starters =
      [ text "    ", colon, text "  " ]
        :: List.repeat (List.length args) [ text "    ", arrow, space ]
  in
    List.map2 (++) starters tipeHtml


longFunctionAnnotationSearch : String -> Name.Dictionary -> List Type -> Type -> List (List Html)
longFunctionAnnotationSearch basePath nameDict args result =
  let
    tipeHtml =
      List.map (Type.toHtmlWithBasePath basePath nameDict Type.Func) (args ++ [ result ])

    starters =
      [ text "    ", colon, text "  " ]
        :: List.repeat (List.length args) [ text "    ", arrow, space ]
  in
    List.map2 (++) starters tipeHtml
